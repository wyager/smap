module Lib (run) where

import Prelude hiding (filter, subtract, init, sin)
import qualified Data.HashMap.Strict as Map (insert, empty, foldrWithKey, alter)
import Data.HashMap.Strict as Map (HashMap, member)
import Data.Hashable (Hashable)
import Control.Lens.At (at) -- for some strange reason I can't import alterF from Data.HashMap.Strict
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Streaming.Char8 as BS8
import qualified Streaming.Prelude as P
import qualified Streaming as S
import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Data.Strict.Tuple (Pair((:!:)))
import qualified Control.Monad.Trans.Resource as Resource
import Control.Monad.IO.Class (MonadIO)
import Crypto.MAC.SipHash (SipKey(..), SipHash(..), hash)
import Data.Word (Word64)
import Flags
  ( Hdl(Std, File)
  , SetDescriptor(Keyed, UnKeyed)
  , Command(Union, Subtract, Intersect, Xor)
  , Accuracy(Approximate, Exact)
  )

force :: Monad m => S.Stream (BS8.ByteString m) m r -> S.Stream (S.Of ByteString) m r
force = S.mapsM BS8.toStrict

duplicate :: forall a m . Monad m => S.Stream (S.Of a) m () -> Stream m a a
duplicate = P.map (\x -> x :!: x)

type Stream m k v = S.Stream (S.Of (Pair k v)) m ()

type SetOperation
  =  forall k
   . (Hashable k, Eq k)
  => [Stream (Resource.ResourceT IO) k ByteString] -- Input maps
  -> Stream (Resource.ResourceT IO) k ByteString -- Output map

cat :: SetOperation
cat streams = foldM filter Map.empty streams *> return ()
 where
  filter seen = P.foldM_ filter' (return seen) return . S.hoist lift
  filter' (seen :: HashMap k ()) (bs :!: v) = case flip at insert bs seen of
    Nothing       -> return seen
    Just inserted -> P.yield (bs :!: v) >> return inserted
   where
    insert Nothing   = Just (Just ())
    insert (Just ()) = Nothing

filterFirstSetWith :: (Bool -> Bool) -> SetOperation
filterFirstSetWith _                []           = return ()
filterFirstSetWith includeIfPresent (first : seconds) = do
  subtract <- lift $ collects seconds
  P.filter (\(k :!: _) -> includeIfPresent (k `member` subtract)) first
 where
  collects = foldM collect Map.empty
  collect subs = P.fold_ (\s (k :!: _) -> Map.insert k () s) subs id

sub :: SetOperation
sub = filterFirstSetWith not

intersect :: SetOperation
intersect = filterFirstSetWith id

xor :: SetOperation
xor = go Map.empty
 where
  go parity []       = Map.foldrWithKey (\k v -> (P.yield (k :!: v) >>)) (return ()) parity
  go parity (x : xs) = do
    parity' <- lift $ P.fold_ toggle parity id x
    go parity' xs
    where toggle hm (k :!: v) = Map.alter (maybe (Just v) (const Nothing)) k hm


keyMap :: Monad m => (k1 -> k2) -> Stream m k1 v -> Stream m k2 v
keyMap f = P.map (\(k :!: v) -> (f k :!: v))

sip :: SipKey -> ByteString -> Word64
sip key bs = let SipHash h = hash key bs in h

hin :: (MonadIO m, Resource.MonadResource m) => Hdl -> BS8.ByteString m ()
hin Std         = BS8.stdin
hin (File path) = BS8.readFile path

sin :: (MonadIO m, Resource.MonadResource m) => SetDescriptor -> Stream m ByteString ByteString
sin (UnKeyed hdl  ) = duplicate $ force $ BS8.lines $ hin hdl
sin (Keyed hks hvs) = S.zipsWith'
  (\q (k P.:> ks) (v P.:> vs) -> (k :!: v) P.:> (q ks vs))
  (f hks)
  (f hvs)
  where f = force . BS8.lines . hin

hout :: (MonadIO m, Resource.MonadResource m) => Hdl -> BS8.ByteString m a -> m a
hout Std         = BS8.stdout
hout (File path) = BS8.writeFile path

format :: Monad m => Stream m k ByteString -> BS8.ByteString m ()
format = BS8.unlines . S.maps (\((_k :!: v) P.:> r) -> BS8.fromStrict v >> return r)

run :: Command -> IO ()
run cmd = case cmd of
  Subtract accuracy p ms o -> withAccuracy accuracy sub (p : ms) o
  Intersect accuracy is o  -> withAccuracy accuracy intersect is o
  Xor       accuracy is o  -> withAccuracy accuracy xor is o
  Union     accuracy is o  -> withAccuracy accuracy cat inputs o
   where
    inputs = case is of
      [] -> [UnKeyed Std]
      xs -> xs
 where
  withAccuracy accuracy (op :: SetOperation) inputs output = case accuracy of
    Exact           -> approximateWith id
    Approximate key -> approximateWith (sip key)
   where
    approximateWith approximator =
      Resource.runResourceT $ hout output $ format $ op $ fmap (keyMap approximator . sin) inputs

