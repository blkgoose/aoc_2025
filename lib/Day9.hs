module Day9 (execute) where

import Data.List.Split (splitOn)
import Data.List (sortBy, sortOn, singleton)
import Flow

import Utils

type Vector = (Int, Int)

execute :: [String] -> (Int, Int)
execute input =
    let [a, b] = map parse input
     in (area a b, 0)
     where
         parse :: String -> Vector
         parse line =
            let line' = splitOn "," line
                x = read (line' !! 0) :: Int
                y = read (line' !! 1) :: Int
            in (x, y)

area :: Vector -> Vector -> Int
area (x1, y1) (x2, y2) =
    let maxX = max x1 x2
        minX = min x1 x2
        maxY = max y1 y2
        minY = min y1 y2
     in (maxX - minX + 1) * (maxY - minY + 1)
