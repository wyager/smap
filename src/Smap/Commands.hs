module Smap.Commands (run) where

import Prelude hiding (filter, subtract, init, sin)
import qualified Data.HashMap.Strict as Map (insert, empty, member)
import Data.HashMap.Strict as Map (HashMap)
import Data.Hashable (Hashable)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Streaming.Char8 as BS8
import qualified Streaming.Prelude as P
import qualified Streaming as S
import Control.Monad (foldM)
import Control.Monad.Morph (hoist, lift)
import Data.Strict.Tuple (Pair((:!:)))
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Control.Monad.Trans.Resource as Resource
import Crypto.MAC.SipHash (SipHash(..), hash)
import Data.ByteString.Builder (word64HexFixed, toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Smap.Flags
  ( Hdl(Std, File)
  , Descriptor(Separate, UnKeyed, Interleaved)
  , Command(Union, Subtract, Intersect)
  , Accuracy(Approximate, Exact)
  )

type Stream m k v = S.Stream (S.Of (Pair k v)) m ()

type RIO = Resource.ResourceT IO

type SetOperation
  =  forall key
   . (Hashable key, Eq key)
  => NonEmpty (Stream RIO key ByteString) -- Input maps
  -> Stream RIO key ByteString -- Output map

cat :: SetOperation
cat streams = foldM filter Map.empty streams *> return ()
 where
  filter seen = P.foldM_ filter' (return seen) return . hoist lift
   -- for some strange reason I can't import alterF from Data.HashMap.Strict
  filter' (seen :: HashMap k ()) (bs :!: v) = if bs `Map.member` seen
    then return seen
    else P.yield (bs :!: v) >> return (Map.insert bs () seen)

filterStreamWith :: (Bool -> Bool) -> SetOperation
filterStreamWith includeIfPresent (first :| seconds) = do
  second <- lift $ collects seconds
  P.filter (\(k :!: _) -> includeIfPresent (k `Map.member` second)) first
 where
  collects = foldM collect Map.empty
  collect subs = P.fold_ (\s (k :!: _) -> Map.insert k () s) subs id

sub :: SetOperation
sub = filterStreamWith not

int :: SetOperation
int = filterStreamWith id

deinterleave :: Monad m => S.Stream (S.Of a) m r -> S.Stream (S.Of (Pair a a)) m r
deinterleave = fmap P.snd' . P.foldM step (return Nothing) return . hoist lift
 where
  step Nothing  a = return (Just a)
  step (Just a) b = P.yield (a :!: b) >> return Nothing

-- Outputs the keys to one file and values to the other (simultaneously)
splitWith
  :: (Pair k ByteString -> P.Stream (P.Of (Either ByteString ByteString)) RIO ())
  -> Hdl
  -> Hdl
  -> Stream RIO k ByteString
  -> RIO ()
splitWith split kFile vFile = hout kFile . hout vFile . hoist unlinify . unlinify . separate
 where
  hout Std         = BS8.stdout
  hout (File path) = BS8.writeFile path
  separate paired = P.partitionEithers $ P.for paired split
  unlinify :: Monad n => S.Stream (S.Of ByteString) n x -> BS8.ByteString n x
  unlinify = BS8.unlines . S.maps (\(b P.:> r) -> BS8.fromStrict b >> return r)

load :: Descriptor ty -> Stream RIO ByteString ByteString
load descriptor = case descriptor of
  UnKeyed     hdl      -> P.map (\x -> x :!: x) (linesOf hdl)
  Interleaved hdl      -> deinterleave (linesOf hdl)
  Separate keys values -> S.zipsWith'
    (\q (k P.:> ks) (v P.:> vs) -> (k :!: v) P.:> (q ks vs))
    (linesOf keys)
    (linesOf values)
 where
  linesOf = S.mapsM BS8.toStrict . BS8.lines . hin
  hin Std         = BS8.stdin
  hin (File path) = BS8.readFile path

withAccuracy
  :: Accuracy
  -> SetOperation
  -> NonEmpty (Stream RIO ByteString ByteString)
  -> Descriptor out
  -> IO ()
withAccuracy accuracy op inputs output = case accuracy of
  Exact           -> approximateWith id id
  Approximate key -> approximateWith
    (\bs -> let SipHash h = hash key bs in h)
    (toStrict . toLazyByteString . word64HexFixed)
 where
  valuesOnly (_k :!: v) = P.yield (Right v)
  separateFiles k2bs (k :!: v) = P.yield (Left (k2bs k)) >> P.yield (Right v)
  sameFile k2bs (k :!: v) = P.yield (Right (k2bs k)) >> P.yield (Right v)
  outputUsing k2bs = case output of
    UnKeyed     hdl -> splitWith valuesOnly hdl hdl
    Interleaved hdl -> splitWith (sameFile k2bs) hdl hdl
    Separate l r    -> splitWith (separateFiles k2bs) l r
  keyMap f = P.map (\(k :!: v) -> (f k :!: v))
  approximateWith approximator k2bs =
    Resource.runResourceT $ outputUsing k2bs $ op $ fmap (keyMap approximator) inputs

run :: Command -> IO ()
run cmd = case cmd of
  Subtract  acc p ms o -> withAccuracy acc sub (load p :| fmap load ms) o
  Intersect acc i is o -> withAccuracy acc int (load i :| fmap load is) o
  Union acc is o       -> withAccuracy acc cat (fmap load inputs) o
   where
    inputs = case is of
      []       -> UnKeyed Std :| []
      (x : xs) -> x :| xs
