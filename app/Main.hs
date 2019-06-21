module Main where

import qualified Lib
import qualified Flags

main :: IO ()
main = do
    command <- Flags.command
    Lib.run command