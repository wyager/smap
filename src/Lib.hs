module Lib
  ( run
  )
where

import           Data.HashMap.Strict as Map
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
                                      , Keyed(Keyed, UnKeyed)
                                      , Command(Cat, Sub)
                                      , Accuracy(Approximate, Exact)
                                    
                                      )

force :: Monad m => P.Stream (BS8.ByteString m) m r -> P.Stream (P.Of ByteString) m r
force = S.mapsM BS8.toStrict

duplicate :: forall a m r . Monad m => P.Stream (P.Of a) m r -> P.Stream (P.Of (Pair a a)) m r
duplicate = P.map (\x -> x :!: x)

cat
  :: forall m k v
   . (Hashable k, Eq k, Monad m)
  => NonEmpty (P.Stream (P.Of (Pair k v)) m ())
  -> P.Stream (P.Of (Pair k v)) m ()
cat streams = foldM filter Map.empty streams *> return ()
 where
  filter
    :: HashMap k ()
    -> P.Stream (P.Of (Pair k v)) m ()
    -> P.Stream (P.Of (Pair k v)) m (HashMap k ())
  filter seen = P.foldM_ filter' (return seen) return . S.hoist lift
  filter' :: HashMap k () -> (Pair k v) -> P.Stream (P.Of (Pair k v)) m (HashMap k ())
  filter' seen (bs :!: v) = case flip at insert bs seen of
    Nothing       -> return seen
    Just inserted -> P.yield (bs :!: v) >> return inserted
   where
    insert Nothing   = Just (Just ())
    insert (Just ()) = Nothing

sub
  :: forall m k v _v
   . (Hashable k, Eq k, Monad m)
  => P.Stream (P.Of (Pair k v)) m ()
  -> [P.Stream (P.Of (Pair k _v)) m ()]
  -> P.Stream (P.Of (Pair k v)) m ()
sub add subs = do
  subtract <- lift $ gathers subs
  P.filter (\(k :!: _) -> not (k `member` subtract)) add
 where
  gathers :: [P.Stream (P.Of (Pair k _v)) m ()] -> m (HashMap k ())
  gathers = foldM gather Map.empty
  gather :: HashMap k () -> P.Stream (P.Of (Pair k _v)) m () -> m (HashMap k ())
  gather subs = P.fold_ (\s (k :!: _) -> Map.insert k () s) subs id

inject
  :: Monad m
  => (k1 -> k2)
  -> (v1 -> v2)
  -> P.Stream (P.Of (Pair k1 v1)) m ()
  -> P.Stream (P.Of (Pair k2 v2)) m ()
inject f g = P.map (\(k :!: v) -> (f k :!: g v))

approximate
  :: Monad m
  => SipKey
  -> P.Stream (P.Of (Pair ByteString v)) m ()
  -> P.Stream (P.Of (Pair Word64 v)) m ()
approximate key = inject (\bs -> let SipHash h = hash key bs in h) id

hin :: (MonadIO m, Resource.MonadResource m) => Hdl -> BS8.ByteString m ()
hin Std         = BS8.stdin
hin (File path) = BS8.readFile path

hout :: (MonadIO m, Resource.MonadResource m) => Hdl -> BS8.ByteString m () -> m ()
hout Std         = BS8.stdout
hout (File path) = BS8.writeFile path

kin
  :: (MonadIO m, Resource.MonadResource m)
  => Keyed
  -> P.Stream (P.Of (Pair ByteString ByteString)) m ()
kin (UnKeyed hdl  ) = duplicate $ force $ BS8.lines $ hin hdl
kin (Keyed hks hvs) = S.zipsWith' (\q (k P.:> ks) (v P.:> vs) -> (k :!: v) P.:> (q ks vs))
                                  (f hks)
                                  (f hvs)
  where f = force . BS8.lines . hin

format :: Monad m => P.Stream (P.Of (Pair k ByteString)) m a -> BS8.ByteString m a
format = BS8.unlines . S.maps (\((_k :!: v) P.:> r) -> BS8.fromStrict v >> return r)

run :: Command -> IO ()
run cmd = 
  case cmd of
    Cat accuracy is o -> case accuracy of
      Exact           -> approximateWith id
      Approximate key -> approximateWith (approximate key)
     where
      approximateWith f = Resource.runResourceT $ hout o $ format $ cat $ fmap (f . kin) inputs
      inputs = case is of
        []       -> (UnKeyed Std) :| []
        (i : is) -> (i :| is)
    Sub accuracy p ms o -> case accuracy of
      Exact           -> approximateWith id
      Approximate key -> approximateWith (approximate key)
     where
      approximateWith f =
        Resource.runResourceT $ hout o $ format $ sub (f (kin p)) (fmap (f . kin) ms)
