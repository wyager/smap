module Main where

import qualified Lib
import qualified Flags

main :: IO ()
main = Lib.run =<< Flags.command