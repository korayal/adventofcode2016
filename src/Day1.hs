{-# LANGUAGE OverloadedStrings #-}
module Day1
    ( solveFirst
    , solveSecond
    ) where

import           Control.Applicative        (pure, (*>), (<$>), (<*>), (<|>))

import qualified Data.Attoparsec.Combinator as AC
import           Data.Attoparsec.Text       (Parser)
import qualified Data.Attoparsec.Text       as A
import           Data.Text                  (Text)
import qualified Data.Text.IO               as I

import           Lib

pos :: Location
pos = Location North 0 0

solveFirst :: String -> IO ()
solveFirst f = do
  l <- I.readFile f
  dse <- pure $ A.parseOnly directions l
  case dse of
    Left _ -> putStrLn "Invalid Input"
    Right ds -> let (Location f x y) = applyDirections pos ds
               in putStrLn $ show $ (abs x) + (abs y)

solveSecond :: String -> IO ()
solveSecond f = do
  l <- I.readFile f
  dse <- pure $ A.parseOnly directions l
  case dse of
    Left _ -> putStrLn "Invalid Input"
    Right ds -> case repeatedLocation (logDirections pos ds) of
      Nothing -> putStrLn "Smart people plan their route first"
      Just (Location f x y) -> putStrLn $ show $ (abs x) + (abs y)

data Direction = DirectionLeft Int | DirectionRight Int | DirectionForward Int deriving (Eq, Show)

-- INPUT PARSERS
direction :: Parser Direction
direction = (left <*> A.decimal) <|>
            (right <*> A.decimal)
  where left = A.char 'L' *> pure DirectionLeft
        right = A.char 'R' *> pure DirectionRight

directions :: Parser [Direction]
directions = direction `AC.sepBy` (A.string ", ")

data Facing = North | East | South | West deriving (Eq, Show, Enum, Bounded)

data Location = Location Facing Int Int deriving (Eq, Show)

moveDirection :: Location -> Direction -> Location
moveDirection (Location f x y) d =
  let newLocation r n = case r of
        North -> Location North x (y + n)
        East -> Location East (x + n) y
        South -> Location South x (y - n)
        West -> Location West (x - n) y
  in case d of
    DirectionLeft n -> newLocation (prev f) n
    DirectionRight n -> newLocation (next f) n
    DirectionForward n -> newLocation f n

applyDirections :: Location -> [Direction] -> Location
applyDirections = foldl moveDirection

logDirections :: Location -> [Direction] -> [Location]
logDirections l d = scanl moveDirection l (concatMap expandDirection d)

expandDirection :: Direction -> [Direction]
expandDirection (DirectionLeft x)
  | x == 1 = [DirectionLeft 1]
  | otherwise = (DirectionLeft 1) : (replicate (x - 1) (DirectionForward 1))
expandDirection (DirectionRight x)
  | x == 1 = [DirectionRight 1]
  | otherwise = (DirectionRight 1) : (replicate (x - 1) (DirectionForward 1))

repeatedLocation :: [Location] -> Maybe Location
repeatedLocation [] = Nothing
repeatedLocation ((Location _ xx yy):xs) = repeatedLocation' [(xx, yy)] xs
  where repeatedLocation' old [] = Nothing
        repeatedLocation' old (x@(Location _ xx2 yy2):xs) = if elem (xx2, yy2) old
                                                            then Just x
                                                            else repeatedLocation' ((xx2, yy2):old) xs
