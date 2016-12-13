module Main where

import qualified Day1 as D1
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args !! 0 of
    "1a" -> D1.solveFirst
    "1b" -> D1.solveSecond
    _    -> putStrLn "Not Yet"
