module Lib
  ( run
  )
where

import           Prelude       hiding ( filter
                                      , subtract
                                      , init
                                      , sin
                                      )
import qualified Data.HashMap.Strict as Map
                                      ( insert
                                      , empty
                                      )
import           Data.HashMap.Strict as Map
                                      ( HashMap
                                      , member
                                      )
import           Data.Hashable        ( Hashable )
import           Control.Lens.At      ( at ) -- for some strange reason I can't import alterF from Data.HashMap.Strict
import           Data.ByteString.Char8
                                      ( ByteString )
import qualified Data.ByteString.Streaming.Char8
                                     as BS8
import qualified Streaming.Prelude   as P
import qualified Streaming           as S
import           Data.List.NonEmpty   ( NonEmpty((:|)) )
import           Control.Monad        ( foldM )
import           Control.Monad.Trans.Class
                                      ( lift )
import           Data.Strict.Tuple    ( Pair((:!:)) )
import qualified Control.Monad.Trans.Resource
                                     as Resource
import           Control.Monad.IO.Class
                                      ( MonadIO )
import           Crypto.MAC.SipHash   ( SipKey(..)
                                      , SipHash(..)
                                      , hash
                                      )
import           Data.Word            ( Word64 )
import           Flags                ( Hdl(Std, File)
                                      , SetDescriptor(Keyed, UnKeyed)
                                      , Command(Union, Subtract, Intersect)
                                      , Accuracy(Approximate, Exact)
                                      )

force :: Monad m => P.Stream (BS8.ByteString m) m r -> P.Stream (P.Of ByteString) m r
force = S.mapsM BS8.toStrict

duplicate :: forall a m r . Monad m => P.Stream (P.Of a) m r -> P.Stream (P.Of (Pair a a)) m r
duplicate = P.map (\x -> x :!: x)


type SetOperation f
  =  forall k
   . (Hashable k, Eq k)
  => f (S.Stream (S.Of (Pair k ByteString)) (Resource.ResourceT IO) ()) -- Input maps
  -> S.Stream (S.Of (Pair k ByteString)) (Resource.ResourceT IO) () -- Output map

cat :: SetOperation NonEmpty
cat streams = foldM filter Map.empty streams *> return ()
 where
  filter seen = P.foldM_ filter' (return seen) return . S.hoist lift
  filter' (seen :: HashMap k ()) (bs :!: v) = case flip at insert bs seen of
    Nothing       -> return seen
    Just inserted -> P.yield (bs :!: v) >> return inserted
   where
    insert Nothing   = Just (Just ())
    insert (Just ()) = Nothing

sub :: SetOperation NonEmpty
sub (pos :| negs) = do
  subtract <- lift $ gathers negs
  P.filter (\(k :!: _) -> not (k `member` subtract)) pos
 where
  gathers = foldM gather Map.empty
  gather subs = P.fold_ (\s (k :!: _) -> Map.insert k () s) subs id

intersect :: SetOperation []
intersect []           = return ()
intersect [x         ] = x
intersect (x : y : zs) = do
  init <- lift $ collapse x
  go init y zs
 where
  collapse = P.fold_ (\hm (k :!: v) -> Map.insert k v hm) Map.empty id
  eliminate hm = P.filter (\(k :!: _) -> k `member` hm)
  go hm s []       = eliminate hm s
  go hm s (t : ts) = do
    hm' <- lift $ collapse $ eliminate hm s
    go hm' t ts

keyMap
  :: Monad m => (k1 -> k2) -> P.Stream (P.Of (Pair k1 v)) m () -> P.Stream (P.Of (Pair k2 v)) m ()
keyMap f = P.map (\(k :!: v) -> (f k :!: v))

sip :: SipKey -> ByteString -> Word64
sip key bs = let SipHash h = hash key bs in h

hin :: (MonadIO m, Resource.MonadResource m) => Hdl -> BS8.ByteString m ()
hin Std         = BS8.stdin
hin (File path) = BS8.readFile path

hout :: (MonadIO m, Resource.MonadResource m) => Hdl -> BS8.ByteString m a -> m a
hout Std         = BS8.stdout
hout (File path) = BS8.writeFile path

sin
  :: (MonadIO m, Resource.MonadResource m)
  => SetDescriptor
  -> P.Stream (P.Of (Pair ByteString ByteString)) m ()
sin (UnKeyed hdl  ) = duplicate $ force $ BS8.lines $ hin hdl
sin (Keyed hks hvs) = S.zipsWith' (\q (k P.:> ks) (v P.:> vs) -> (k :!: v) P.:> (q ks vs))
                                  (f hks)
                                  (f hvs)
  where f = force . BS8.lines . hin

format :: Monad m => P.Stream (P.Of (Pair k ByteString)) m a -> BS8.ByteString m a
format = BS8.unlines . S.maps (\((_k :!: v) P.:> r) -> BS8.fromStrict v >> return r)

run :: Command -> IO ()
run cmd = case cmd of
  Subtract accuracy p ms o -> withAccuracy accuracy sub (p :| ms) o
  Intersect accuracy is o  -> withAccuracy accuracy intersect is o
  Union     accuracy is o  -> withAccuracy accuracy cat inputs o
   where
    inputs = case is of
      []         -> (UnKeyed Std) :| []
      (i : rest) -> (i :| rest)
 where
  withAccuracy accuracy (g :: SetOperation f) i o = case accuracy of
    Exact           -> approximateWith id
    Approximate key -> approximateWith (sip key)
    where approximateWith f = Resource.runResourceT $ hout o $ format $ g $ fmap (keyMap f . sin) i

