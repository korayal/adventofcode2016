{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Day3
    ( solveFirst
    , solveSecond
    ) where

import           Control.Applicative        (pure, (*>), (<$>), (<*>), (<|>))
import qualified Data.Attoparsec.Combinator as AC
import           Data.Attoparsec.Text       (Parser)
import qualified Data.Attoparsec.Text       as A
import           Data.Text                  (Text, snoc)
import qualified Data.Text.IO               as I
import           Lib

solveFirst :: String -> IO ()
solveFirst f = do
  l <- I.readFile f
  dse <- pure $ A.parseOnly triangles l
  case dse of
    Left _ -> putStrLn "Invalid Input"
    Right ds -> (putStrLn . show . length) (filter validTriangle ds)

solveSecond :: String -> IO ()
solveSecond f = do
  l <- I.readFile f
  dse <- pure $ A.parseOnly cTriangles l
  case dse of
    Left _ -> putStrLn "Invalid Input"
    Right ds -> (putStrLn . show . length) (filter validTriangle ds)

data Triangle = Triangle { aSide :: Int
                         , bSide :: Int
                         , cSide :: Int
                         } deriving (Eq, Show)

-- INPUT PARSERS
triangle :: Parser Triangle
triangle = do
  A.skipMany (A.char ' ')
  a <- A.decimal
  A.skipMany (A.char ' ')
  b <- A.decimal
  A.skipMany (A.char ' ')
  c <- A.decimal
  return (Triangle a b c)

triangles :: Parser [Triangle]
triangles = triangle `AC.sepBy` A.endOfLine

cTriangle :: Parser [Triangle]
cTriangle = do
  ta <- triangle
  A.endOfLine
  tb <- triangle
  A.endOfLine
  tc <- triangle
  A.endOfLine
  return [ Triangle (aSide ta) (aSide tb) (aSide tc)
         , Triangle (bSide ta) (bSide tb) (bSide tc)
         , Triangle (cSide ta) (cSide tb) (cSide tc)
         ]

cTriangles :: Parser [Triangle]
cTriangles = fmap concat (A.many1 cTriangle)

validTriangle :: Triangle -> Bool
validTriangle Triangle {..} =
  ((aSide + bSide) > cSide) && ((aSide + cSide) > bSide) && ((bSide + cSide) > aSide)
