module Lib ( prev
           , next
           ) where

prev :: (Eq a, Enum a, Bounded a) => a -> a
prev e | e == minBound = maxBound
       | otherwise = pred e

prev' :: (Eq a, Enum a, Bounded a) => a -> a
prev' e | e == minBound = minBound
       | otherwise = pred e

next :: (Eq a, Enum a, Bounded a) => a -> a
next e | e == maxBound = minBound
       | otherwise = succ e

next' :: (Eq a, Enum a, Bounded a) => a -> a
next' e | e == maxBound = maxBound
       | otherwise = succ e
