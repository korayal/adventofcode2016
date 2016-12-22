module Main where

import qualified Day1 as D1
import qualified Day2 as D2
import qualified Day3 as D3
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args !! 0 of
    "1a" -> D1.solveFirst $ args !! 1
    "1b" -> D1.solveSecond $ args !! 1
    "2a" -> D2.solveFirst $ args !! 1
    "2b" -> D2.solveSecond $ args !! 1
    "3a" -> D3.solveFirst $ args !! 1
    "3b" -> D3.solveSecond $ args !! 1
    _    -> putStrLn "Not Yet"
