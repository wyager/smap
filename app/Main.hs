module Main where

import Smap.Commands (run)
import Smap.Flags (command)

main :: IO ()
main = run =<< command
