module Main where

import Lib
import ParserGenerator
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  exec filename
  return ()

