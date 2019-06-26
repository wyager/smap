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
import Control.Monad.Trans.Class (lift)
import Data.Strict.Tuple (Pair((:!:)))
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Control.Monad.Trans.Resource as Resource
import Control.Monad.IO.Class (MonadIO)
import Crypto.MAC.SipHash (SipHash(..), hash)
import Smap.Flags
  ( Hdl(Std, File)
  , Descriptor(Keyed, UnKeyed)
  , Command(Union, Subtract, Intersect)
  , Accuracy(Approximate, Exact)
  )

type Stream m k v = S.Stream (S.Of (Pair k v)) m ()

type SetOperation
  =  forall key
   . (Hashable key, Eq key)
  => NonEmpty (Stream (Resource.ResourceT IO) key ByteString) -- Input maps
  -> Stream (Resource.ResourceT IO) key ByteString -- Output map

cat :: SetOperation
cat streams = foldM filter Map.empty streams *> return ()
 where
  filter seen = P.foldM_ filter' (return seen) return . S.hoist lift
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

intersect :: SetOperation
intersect = filterStreamWith id

load :: (MonadIO m, Resource.MonadResource m) => Descriptor ty -> Stream m ByteString ByteString
load descriptor = case descriptor of
  UnKeyed hdl       -> P.map (\x -> x :!: x) (linesOf hdl)
  Keyed keys values -> S.zipsWith'
    (\q (k P.:> ks) (v P.:> vs) -> (k :!: v) P.:> (q ks vs))
    (linesOf keys)
    (linesOf values)
 where
  linesOf = S.mapsM BS8.toStrict . BS8.lines . hin
  hin Std         = BS8.stdin
  hin (File path) = BS8.readFile path

run :: Command -> IO ()
run cmd = case cmd of
  Subtract  accuracy p ms o -> withAccuracy accuracy sub (load p :| fmap load ms) o
  Intersect accuracy i is o -> withAccuracy accuracy intersect (load i :| fmap load is) o
  Union accuracy is o       -> withAccuracy accuracy cat (fmap load inputs) o
   where
    inputs = case is of
      []       -> UnKeyed Std :| []
      (x : xs) -> x :| xs
 where
  withAccuracy accuracy (op :: SetOperation) inputs output = case accuracy of
    Exact           -> approximateWith id
    Approximate key -> approximateWith (sip key)
   where
    format = BS8.unlines . S.maps (\((_k :!: v) P.:> r) -> BS8.fromStrict v >> return r)
    hout Std         = BS8.stdout
    hout (File path) = BS8.writeFile path
    sip key bs = let SipHash h = hash key bs in h
    keyMap f = P.map (\(k :!: v) -> (f k :!: v))
    approximateWith approximator =
      Resource.runResourceT $ hout output $ format $ op $ fmap (keyMap approximator) inputs