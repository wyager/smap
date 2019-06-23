module Flags
  ( Hdl(Std, File)
  , SetDescriptor(Keyed, UnKeyed)
  , Command(Union, Subtract, Intersect, Xor)
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

data SetDescriptor = Keyed Hdl Hdl -- Keys are in the first file, values in the second (map behavior)
                   | UnKeyed Hdl -- key = value (set behavior)

data Accuracy = Approximate SipKey -- Don't keep the actual value as a key, just its hash
              | Exact -- Keep the actual value as a key

data Command = Union Accuracy [SetDescriptor] Hdl
             | Subtract Accuracy SetDescriptor [SetDescriptor] Hdl
             | Intersect Accuracy [SetDescriptor] Hdl
             | Xor Accuracy [SetDescriptor] Hdl

hdl :: A.Parser Hdl
hdl = stdin <|> path
 where
  stdin = Std <$ A.char '-'
  path  = File . Text.unpack <$> A.takeWhile (/= ',')

setExplanation :: String
setExplanation = "Use '-' for stdin. Use +keyfile,valfile for separate keys and values."

multiSetExplanation :: String
multiSetExplanation = "Can specify 0 or more files. " ++ setExplanation

outHdl :: O.Parser Hdl
outHdl = O.option
  (aToO hdl)
  (O.metavar "OUTFILE" <> O.short 'o' <> O.long "out" <> O.value Std <> O.help "Defaults to stdout."
  )

inFiles :: O.Parser [SetDescriptor]
inFiles = many $ O.argument (aToO setDescriptor) (O.metavar "FILE*" <> O.help multiSetExplanation)

setDescriptor :: A.Parser SetDescriptor
setDescriptor =
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

accuracy :: O.Parser Accuracy
accuracy = approx <|> exact
 where
  approx = O.flag'
    (Approximate (SipKey 0 0))
    (O.short 'a' <> O.long "approximate" <> O.help
      (concat
        [ "For deduplication, store a 64-bit siphash rather than the whole line. "
        , "Can save memory, depending on the set operation"
        ]
      )
    )
  exact = pure Exact

catCommand :: O.Mod O.CommandFields Command
catCommand = O.command "cat" $ O.info
  (value O.<**> O.helper)
  (O.fullDesc <> O.progDesc "Set union. Output everything from the input sets, deduplicated.")
  where value = Union <$> accuracy <*> inFiles <*> outHdl

subCommand :: O.Mod O.CommandFields Command
subCommand = O.command "sub" $ O.info
  (value O.<**> O.helper)
  (O.fullDesc <> O.progDesc
    (concat
      [ "Set subtraction. "
      , "Given stream A and sets B,C,..., output values in A but not in B,C,... "
      , "Does not deduplicate values from A."
      ]
    )
  )
 where
  value = Subtract <$> accuracy <*> plus <*> minuses <*> outHdl
  plus  = O.argument (aToO setDescriptor) (O.metavar "PLUSFILE" <> O.help setExplanation)
  minuses =
    many $ O.argument (aToO setDescriptor) (O.metavar "MINUSFILE*" <> O.help multiSetExplanation)

intersectCommand :: O.Mod O.CommandFields Command
intersectCommand = O.command "int" $ O.info
  (value O.<**> O.helper)
  (O.fullDesc <> O.progDesc
    "Set intersection. Output values which are in all input sets. Deduplicates values."
  )
  where value = Intersect <$> accuracy <*> inFiles <*> outHdl

xorCommand :: O.Mod O.CommandFields Command
xorCommand = O.command "xor" $ O.info
  (value O.<**> O.helper)
  (O.fullDesc <> O.progDesc
    (concat
      [ "Set xor/symmetric difference. "
      , "Given files A and B, output all values which are A or B but not both. "
      , "Given more than 2 files, output everything that appears in an odd number of files."
      ]
    )
  )
  where value = Xor <$> accuracy <*> inFiles <*> outHdl

command :: IO Command
command = O.execParser
  (O.info
    ((O.subparser (catCommand <> subCommand <> intersectCommand <> xorCommand)) O.<**> O.helper)
    (O.fullDesc <> O.progDesc "Use -h for help. Available commands: cat, sub, int, xor.")
  )
