module Lib
  ( main
  )
where

import           Data.HashMap.Strict           as Map
                                                ( insert
                                                , empty
                                                )
import           Data.HashMap.Strict           as Map
                                                ( HashMap )
import           Data.Hashable                  ( Hashable )
import           Control.Lens.At                ( at ) -- for some strange reason I can't import alterF from Data.HashMap.Strict
import           Data.ByteString.Char8          ( ByteString )
import qualified Data.ByteString.Streaming.Char8
                                               as BS8
import qualified Streaming.Prelude             as P
import qualified Streaming                     as S
import           Data.List.NonEmpty             ( NonEmpty((:|)) )
import           Control.Monad                  ( foldM )
import           Control.Monad.Trans.Class      ( lift )
import           Data.Strict.Tuple              ( Pair((:!:)) )
import qualified Options.Applicative           as O
import           Control.Applicative            ( many
                                                , (<|>)
                                                )
import qualified Data.Attoparsec.Text          as A
import qualified Data.Text                     as Text
import qualified Control.Monad.Trans.Resource  as Resource
import           Control.Monad.IO.Class         ( MonadIO )
import           Crypto.MAC.SipHash             ( SipKey(..)
                                                , SipHash(..)
                                                , hash
                                                )
import           Data.Word                      ( Word64 )



gatherSet
  :: (Monad m, Hashable a, Eq a) => P.Stream (P.Of a) m r -> m (HashMap a a)
gatherSet = P.fold_ (\m k -> Map.insert k k m) Map.empty id

{-# INLINE force #-}
force
  :: Monad m
  => P.Stream (BS8.ByteString m) m r
  -> P.Stream (P.Of ByteString) m r
force = S.mapsM BS8.toStrict

{-# INLINE duplicate #-}
duplicate
  :: forall a m r
   . Monad m
  => P.Stream (P.Of a) m r
  -> P.Stream (P.Of (Pair a a)) m r
duplicate = P.map (\x -> x :!: x)

{-# INLINE cat #-}
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
  filter seen =
    fmap P.fst' . P.foldM filter' (return seen) return . S.hoist lift
  filter'
    :: HashMap k () -> (Pair k v) -> P.Stream (P.Of (Pair k v)) m (HashMap k ())
  filter' seen (bs :!: v) = case flip at insert bs seen of
    Nothing       -> return seen
    Just inserted -> P.yield (bs :!: v) >> return inserted
   where
    insert Nothing   = Just (Just ())
    insert (Just ()) = Nothing

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



data Hdl = Std | File FilePath deriving Show

hin :: (MonadIO m, Resource.MonadResource m) => Hdl -> BS8.ByteString m ()
hin Std         = BS8.stdin
hin (File path) = BS8.readFile path

hout
  :: (MonadIO m, Resource.MonadResource m) => Hdl -> BS8.ByteString m () -> m ()
hout Std         = BS8.stdout
hout (File path) = BS8.writeFile path

data Keyed = Keyed Hdl Hdl | UnKeyed Hdl deriving Show

kin
  :: (MonadIO m, Resource.MonadResource m)
  => Keyed
  -> P.Stream (P.Of (Pair ByteString ByteString)) m ()
kin (UnKeyed hdl  ) = duplicate $ force $ BS8.lines $ hin hdl
kin (Keyed hks hvs) = S.zipsWith'
  (\q (k P.:> ks) (v P.:> vs) -> (k :!: v) P.:> (q ks vs))
  (f hks)
  (f hvs)
  where f = force . BS8.lines . hin

hdl :: A.Parser Hdl
hdl = stdin <|> path
 where
  stdin = Std <$ A.char '-'
  path  = File . Text.unpack <$> A.takeWhile (/= ',')


keyed :: A.Parser Keyed
keyed =
  (keyed <|> unkeyed)
    <*    (A.endOfInput A.<?> "more filepath characters than expected")
    A.<?> "Could not parse filepath"
 where
  unkeyed = UnKeyed <$> hdl
  keyed   = do
    _ <- A.char '+'
    k <- hdl
    _ <- A.char ','
    v <- hdl
    return (Keyed k v)

aToO :: A.Parser a -> O.ReadM a
aToO p = O.eitherReader (A.parseOnly p . Text.pack)


data Command = Cat Accuracy [Keyed] Hdl deriving Show

catI :: O.Mod O.CommandFields Command
catI = O.command "cat" $ O.info (value O.<**> O.helper) O.fullDesc
 where
  value = Cat <$> accuracy <*> many in_ <*> out
  in_   = O.argument
    (aToO keyed)
    (  O.metavar "INFILE"
    <> O.help
         "Can specify 0 or more files. Use '-' for stdin. Use +keyfile,valfile for separate keys and values. Uses stdin if none specified."
    )
  out = O.option
    (aToO hdl)
    (O.metavar "OUTFILE" <> O.short 'o' <> O.long "out" <> O.value Std <> O.help
      "Defaults to stdout."
    )

data Accuracy = Approximate SipKey | Exact
instance Show Accuracy where
  show Exact           = "Exact"
  show (Approximate _) = "Approximate"

accuracy :: O.Parser Accuracy
accuracy = approx <|> exact
 where
  approx = O.flag'
    (Approximate (SipKey 0 0))
    (  O.short 'a'
    <> O.long "approximate"
    <> O.help
         "For deduplication, store a 64-bit siphash rather than the whole line. Can save memory"
    )
  exact = pure Exact

format
  :: Monad m => P.Stream (P.Of (Pair k ByteString)) m a -> BS8.ByteString m a
format =
  BS8.unlines . S.maps (\((_k :!: v) P.:> r) -> BS8.fromStrict v >> return r)

main = do
  cmd <- O.execParser (O.info ((O.subparser catI) O.<**> O.helper) O.fullDesc)
  case cmd of
    Cat accuracy is o -> case accuracy of
      Exact           -> approximateWith id
      Approximate key -> approximateWith (approximate key)
     where
      approximateWith f =
        Resource.runResourceT $ hout o $ format $ cat $ fmap (f . kin) inputs
      inputs = case is of
        []       -> (UnKeyed Std) :| []
        (i : is) -> (i :| is)

