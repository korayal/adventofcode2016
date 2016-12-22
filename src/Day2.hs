{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Day2
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
  dse <- pure $ A.parseOnly kpLines l
  case dse of
    Left _ -> putStrLn "Invalid Input"
    Right ds -> I.putStrLn $ moveInput keyPad initialPosition ds

solveSecond :: String -> IO ()
solveSecond f = do
  l <- I.readFile f
  dse <- pure $ A.parseOnly kpLines l
  case dse of
    Left _ -> putStrLn "Invalid Input"
    Right ds -> I.putStrLn $ moveInput realKeypad realInitialPosition ds

-- INPUT PARSERS

data KPDirection = KPUp | KPRight | KPDown | KPLeft deriving (Eq, Show)

data KPPosition = KPPosition { xPosition :: Int
                             , yPosition :: Int
                             } deriving (Eq, Show)

initialPosition :: KPPosition
initialPosition = KPPosition 1 1

realInitialPosition :: KPPosition
realInitialPosition = KPPosition 0 2

type KeyPad = [[Char]]

keyPad :: KeyPad
keyPad = [ "147"
         , "258"
         , "369"]

realKeypad :: KeyPad
realKeypad = [ "  5  "
             , " 26A "
             , "137BD"
             , " 48C "
             , "  9  "]

kpDirection :: Parser KPDirection
kpDirection = left <|> right <|> up <|> down
  where left = A.char 'L' *> pure KPLeft
        right = A.char 'R' *> pure KPRight
        up = A.char 'U' *> pure KPUp
        down = A.char 'D' *> pure KPDown

kpDirections :: Parser [KPDirection]
kpDirections = A.many1 kpDirection

kpLines :: Parser [[KPDirection]]
kpLines = kpDirections `AC.sepBy` A.endOfLine

moveDirection :: KeyPad -> KPPosition -> KPDirection -> KPPosition
moveDirection kp (p@KPPosition {..}) d = case d of
  KPLeft  -> if (xPosition /= 0 && ((kp !! yPosition) !! (xPosition - 1)) /= ' ')
            then KPPosition (xPosition - 1) yPosition
            else p
  KPRight -> if (xPosition /= (length kp - 1) && ((kp !! yPosition) !! (xPosition + 1)) /= ' ')
            then KPPosition (xPosition + 1) yPosition
            else p
  KPUp    -> if (yPosition /= 0 && ((kp !! (yPosition - 1)) !! (xPosition)) /= ' ')
            then KPPosition xPosition (yPosition - 1)
            else p
  KPDown  -> if (yPosition /= (length kp - 1) && ((kp !! (yPosition + 1)) !! xPosition) /= ' ')
            then KPPosition xPosition (yPosition + 1)
            else p

moveRow :: KeyPad -> KPPosition -> [KPDirection] -> (Char, KPPosition)
moveRow kp p dl = let finalPosition = foldl (moveDirection kp) p dl
                      finalKey = (kp !! (xPosition finalPosition)) !! (yPosition finalPosition)
                  in (finalKey, finalPosition)

-- TODO add a helper function to extract stuff
moveInput :: KeyPad -> KPPosition -> [[KPDirection]] -> Text
moveInput kp p dll = let go :: KeyPad -> KPPosition -> [KPDirection] -> Text -> (Text, KPPosition)
                         go kp' pos' d' acc = (snoc acc c, pos) where (c, pos) = moveRow kp pos' d'
                         finalDigits = foldl (\(acc, pos) d -> go kp pos d acc) ("", p) dll
                     in fst finalDigits
