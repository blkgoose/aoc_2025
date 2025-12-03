module Utils where

import qualified Debug.Trace as Debug

trace :: (Show a) => a -> a
trace x = Debug.trace (show x) x

range :: Int -> Int -> [Int]
range a b
  | a <= b    = [a .. b]
  | otherwise = [a, a-1 .. b]
