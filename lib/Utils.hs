module Utils where

import qualified Debug.Trace as Debug
import Data.Char (isSpace)
import Data.Array

type Coord = (Int, Int)
type Grid a = Array Coord a

trace :: (Show a) => a -> a
trace x = Debug.trace (show x) x

trace' :: (Show a) => a -> b -> b
trace' x v = Debug.trace (show x) v

range :: Int -> Int -> [Int]
range a b
  | a <= b    = [a .. b]
  | otherwise = [a, a-1 .. b]

groupWhile :: (a -> Bool) -> [a] -> [[a]]
groupWhile pred line = reverse $ foldl (step) [] line
    where
        step [] x = [[x]]
        step (g:gs) x
            | pred x    = (g ++ [x]) : gs
            | otherwise = [] : (g:gs)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

makeGrid :: [[a]] -> Grid a
makeGrid rows@(header:_) =
    let h = length rows
        w = length header
        spots = [((i,j), rows !! i !! j) | i <- [0..h-1], j <- [0..w-1]]
    in array ((0,0),(h-1,w-1)) spots
