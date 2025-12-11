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
    |> \((_, _, area):_) -> area

part2 :: [Vector] -> Int
part2 points =
  orderByLargestArea points
    |> filter (\(a, b, _) -> not $ intersectPolygon (a, b) (polygon points))
    |> Utils.traceList "Points:"
    |> \((_, _, area):_) -> area

findInterections :: Vector -> Vector -> [(Vector, Vector)] -> [(Vector, Vector)]
findInterections a b polygonLines =
   filter (intesect (a, b)) polygonLines

intersectPolygon :: (Vector, Vector) -> [(Vector, Vector)] -> Bool
intersectPolygon line polygonLines =
  any (intesect line) polygonLines

intesect :: (Vector, Vector) -> (Vector, Vector) -> Bool
intesect ((ax, ay), (bx, by)) edge@((ex, ey), _) =
   let minX = min ax bx
       maxX = max ax bx
       minY = min ay by
       maxY = max ay by
   in if isVertical edge
      then ex > minX && ex < maxX
      else ey > minY && ey < maxY
  where
    isVertical ((x1, _), (x2, _)) = x1 == x2

polygon :: [Vector] -> [(Vector, Vector)]
polygon points@(x:t) = zip points (t ++ [x])

orderByLargestArea :: [Vector] -> [(Vector, Vector, Int)]
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
