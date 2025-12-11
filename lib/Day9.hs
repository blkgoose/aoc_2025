module Day9 (execute) where

import Data.List.Split (splitOn)
import Data.List (sortBy, sortOn, singleton)
import Flow

import Utils

type Vector = (Int, Int)

execute :: [String] -> (Int, Int)
execute input =
    let p = map parse input
     in (part1 p, 0)
     where
         parse :: String -> Vector
         parse line =
            let line' = splitOn "," line
                x = read (line' !! 0) :: Int
                y = read (line' !! 1) :: Int
            in (x, y)

part1 :: [Vector] -> Int
part1 p =
    [(a, b, area a b) | a <- p, b <- p, a < b]
        |> sortBy (\(_, _, area1) (_, _, area2) -> compare area2 area1)
        |> \((_, _, a) : _) -> a

area :: Vector -> Vector -> Int
area (x1, y1) (x2, y2) =
    let maxX = max x1 x2
        minX = min x1 x2
        maxY = max y1 y2
        minY = min y1 y2
     in (maxX - minX + 1) * (maxY - minY + 1)
