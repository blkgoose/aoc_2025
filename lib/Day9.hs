module Day9 (execute) where

import Data.List.Split (splitOn)
import Data.List (sortBy, sortOn, singleton)
import Flow

import Utils

type Vector = (Int, Int)

execute :: [String] -> (Int, Int)
execute input =
    let points = map parse input
     in (part1 points, 0)
     where
         parse :: String -> Vector
         parse line =
            let line' = splitOn "," line
                x = read (line' !! 0) :: Int
                y = read (line' !! 1) :: Int
            in (x, y)

part1 :: [Vector] -> Int
part1 points =
    orderByLargestArea points
    |> take 1
    |> \[(_, _, area)] -> area
    
orderByLargestArea points = 
    [(a, b, area a b) | a <- points, b <- points, a < b]
        |> sortBy (\(_, _, area1) (_, _, area2) -> compare area2 area1)

area :: Vector -> Vector -> Int
area (x1, y1) (x2, y2) =
    let maxX = max x1 x2
        minX = min x1 x2
        maxY = max y1 y2
        minY = min y1 y2
     in (maxX - minX + 1) * (maxY - minY + 1)
