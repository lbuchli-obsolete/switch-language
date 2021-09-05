module Main where

import Lib (run)
import System.IO

file :: String
file = "/home/lukas/workspace/haskell/switch-language/lang-src/test.sw"

main :: IO ()
main = do
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  print $ run contents
