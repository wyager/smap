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

cat_main :: IO ()
cat_main =
  BS8.stdout
    $ BS8.unlines
    $ S.maps
        (\((_k :!: v) P.:> r) ->
          BS8.fromStrict v >> BS8.singleton '\n' >> return r
        )
    $ cat
    $ (:| [])
    $ duplicate
    $ force
    $ BS8.lines BS8.stdin

data Hdl = Std | File FilePath deriving Show

hin :: (MonadIO m, Resource.MonadResource m) => Hdl -> BS8.ByteString m ()
hin Std         = BS8.stdin
hin (File path) = BS8.readFile path

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



keyed :: A.Parser Keyed
keyed =
  (keyed <|> unkeyed)
    <*    (A.endOfInput A.<?> "more filepath characters than expected")
    A.<?> "Could not parse filepath"
 where
  file    = stdin <|> path
  stdin   = Std <$ A.char '-'
  path    = File . Text.unpack <$> A.takeWhile (/= ',')
  unkeyed = UnKeyed <$> file
  keyed   = do
    _ <- A.char '+'
    k <- file
    _ <- A.char ','
    v <- file
    return (Keyed k v)

readKeyed :: O.ReadM Keyed
readKeyed = O.eitherReader (A.parseOnly keyed . Text.pack)

data Command = Cat Accuracy Keyed [Keyed] Keyed deriving Show

catI :: O.Mod O.CommandFields Command
catI = O.command "cat" $ O.info (value O.<**> O.helper) O.fullDesc
 where
  value = Cat <$> accuracy <*> in_ <*> many in_ <*> out
  in_   = O.argument readKeyed (O.metavar "INFILE")
  out   = O.option
    readKeyed
    (  O.showDefaultWith
        (\case
          (UnKeyed Std) -> "stdout"
          _             -> error "Program error: default changed"
        )
    <> O.metavar "OUTFILE"
    <> O.short 'o'
    <> O.long "out"
    <> O.value (UnKeyed Std)
    )

data Accuracy = Approximate SipKey | Exact
instance Show Accuracy where
  show Exact           = "Exact"
  show (Approximate _) = "Approximate"

accuracy :: O.Parser Accuracy
accuracy = approx <|> exact
 where
  approx =
    O.flag' (Approximate (SipKey 0 0)) (O.short 'a' <> O.long "approximate")
  exact = pure Exact


main = do
  cmd <- O.execParser (O.info ((O.subparser catI) O.<**> O.helper) O.idm)
  case cmd of
    Cat accuracy i is o ->
      let approximateWith f =
              Resource.runResourceT
                $ BS8.stdout
                $ BS8.unlines
                $ S.maps (\((_k :!: v) P.:> r) -> BS8.fromStrict v >> return r)
                $ cat (fmap (f . kin) (i :| is))
      in  case accuracy of
            Exact           -> approximateWith id
            Approximate key -> approximateWith (approximate key)

