module Main where

import System.Environment (getArgs)
import Test.DocTest

main :: IO ()
main = do
  args <- getArgs
  doctest $ case args of
    [] -> ["src"]
    fs -> fs
