-- Command line flag parsing 

module Smap.Flags
  ( Hdl(Std, File)
  , Descriptor(Separate, UnKeyed, Interleaved)
  , FileType(Set, Stream)
  , Command(Union, Subtract, Intersect)
  , Accuracy(Approximate, Exact)
  , command
  )
where

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as Text
import qualified Options.Applicative as O
import Control.Applicative (many, (<|>))
import Crypto.MAC.SipHash (SipKey(..))

data Hdl = Std -- stdin/stdout
         | File FilePath

-- Just to help us keep track of how a descriptor is used
data FileType = Set -- Duplicates are discarded
              | Stream -- Duplicates are not discarded

data Descriptor (ty :: FileType)
   = Separate Hdl Hdl -- Keys are in the first file, values in the second (map behavior)
   | Interleaved Hdl -- Keys and values are on separate lines in the same file
   | UnKeyed Hdl -- key = value (set behavior)

data Accuracy = Approximate SipKey -- Don't keep the actual value as a key, just its hash
              | Exact -- Keep the actual value as a key

data Command = Union Accuracy [Descriptor 'Set] (Descriptor 'Set)
             | Subtract Accuracy (Descriptor 'Stream) [Descriptor 'Set] (Descriptor 'Stream)
             | Intersect Accuracy (Descriptor 'Stream) [Descriptor 'Set] (Descriptor 'Stream)

hdl :: A.Parser Hdl
hdl = stdin <|> path
 where
  stdin = Std <$ A.char '-'
  path  = File . Text.unpack <$> A.takeWhile (/= ',')

descriptor :: A.Parser (Descriptor ty)
descriptor =
  (keyed <|> interleaved <|> unkeyed)
    <*    (A.endOfInput A.<?> "more filepath characters than expected")
    A.<?> "Could not parse filepath"
 where
  unkeyed = UnKeyed <$> hdl
  keyed   = do
    _ <- A.char '+'
    k <- hdl
    _ <- A.char ','
    v <- hdl
    return (Separate k v)
  interleaved = Interleaved <$> (A.char '@' *> hdl)


aToO :: A.Parser a -> O.ReadM a
aToO p = O.eitherReader (A.parseOnly p . Text.pack)

argDescription :: String
argDescription =
  "Use '-' for stdin/out. Use +keyfile,valfile for separate keys and values. Use @file for keys and values on alternating lines."

stream :: O.Parser (Descriptor 'Stream)
stream = O.argument
  (aToO descriptor)
  (O.metavar "STREAM" <> O.help ("Stream source file. " ++ argDescription))

output :: O.Parser (Descriptor a)
output =
  O.option
      (aToO descriptor)
      (  O.metavar "OUTPUT"
      <> O.help ("Output file(s). Defaults to stdout. " ++ argDescription)
      <> O.short 'o'
      <> O.long "output"
      )
    <|> pure (UnKeyed Std)

sets :: O.Parser [Descriptor 'Set]
sets = many $ O.argument
  (aToO descriptor)
  (O.metavar "SET*" <> O.help ("Set/map source files (0 or more). " ++ argDescription))

accuracy :: O.Parser Accuracy
accuracy = approx <|> approxWithKey <|> exact
 where
  approx = O.flag'
    (Approximate (SipKey 0 0))
    (  O.short 'a'
    <> O.long "approximate"
    <> O.help
         "For deduplication, store a 64-bit siphash rather than the whole line. Can save memory, depending on the set operation"
    )

  approxWithKey = O.option
    (fmap
      (\i -> Approximate $ SipKey (fromIntegral $ i `div` (2 ^ (64 :: Integer))) (fromIntegral i))
      (O.auto :: O.ReadM Integer)
    )
    (O.short 'k' <> O.long "approx-with-key" <> O.metavar "KEY" <> O.help
      "like --approximate, but takes a 128-bit SipHash key. Example: -k 0x1234..."
    )
  exact = pure Exact

catCommand :: O.Mod O.CommandFields Command
catCommand = O.command "cat" $ O.info
  (value O.<**> O.helper)
  (O.fullDesc <> O.progDesc "Set union. Output everything from the input sets, deduplicated.")
  where value = Union <$> accuracy <*> sets <*> output

subCommand :: O.Mod O.CommandFields Command
subCommand = O.command "sub" $ O.info
  (value O.<**> O.helper)
  (O.fullDesc <> O.progDesc
    (concat
      [ "Set subtraction. "
      , "Given stream A and sets B,C,..., output values in A but not in any of B,C,... "
      , "Does not deduplicate values from A."
      ]
    )
  )
  where value = Subtract <$> accuracy <*> stream <*> sets <*> output

intersectCommand :: O.Mod O.CommandFields Command
intersectCommand = O.command "int" $ O.info
  (value O.<**> O.helper)
  (O.fullDesc <> O.progDesc
    (concat
      [ "Set intersection. "
      , "Given stream A and sets B,C,..., output values in A "
      , "only if they are in all of of B,C,... "
      , "Does not deduplicate values from A."
      ]
    )
  )
  where value = Intersect <$> accuracy <*> stream <*> sets <*> output

command :: IO Command
command = O.execParser
  (O.info
    ((O.subparser (catCommand <> subCommand <> intersectCommand)) O.<**> O.helper)
    (O.fullDesc <> O.progDesc "Use -h for help. Available commands: cat, sub, int.")
  )
