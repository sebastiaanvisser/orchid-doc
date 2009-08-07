module Main where

import System.Environment

import Doc.Json
import Doc.JsonParser
import Misc.Commons

main = do
  args <- getArgs
  file <- readFile (args !! 0)
  let a = maybe "invalid json" sem_Json (parse_json @@ file)
  putStrLn a

