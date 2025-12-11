module Day9 (execute) where

import Data.List (singleton, sortBy, sortOn)
import Data.List.Split (splitOn)
import Flow
import Utils

type Vector = (Int, Int)

execute :: [String] -> (Int, Int)
execute input =
  let points = map parse input
   in (part1 points, part2 points)
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

part2 :: [Vector] -> Int
part2 points =
  orderByLargestArea points
    |> map (\(a, b, a') -> (a, b, a', not $ intersectsAny (a, b) (pointsWithout (a, b))))
    |> Utils.traceList "Areas with no intersections:"
    |> flip Utils.trace' 0
  where
    pointsWithout :: (Vector, Vector) -> [Vector]
    pointsWithout (a, b) =
      filter (\p -> p /= a && p /= b) points

intersectsAny :: (Vector, Vector) -> [Vector] -> Bool
intersectsAny bounds points =
  any (\point -> instesect bounds point) points

instesect :: (Vector, Vector) -> Vector -> Bool
instesect bound point =
  let (a, b) = bound
      (px, py) = point
      minX = min (fst a) (fst b)
      maxX = max (fst a) (fst b)
      minY = min (snd a) (snd b)
      maxY = max (snd a) (snd b)
   in px >= minX && px <= maxX && py >= minY && py <= maxY

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
