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
    |> \((_, _, area):_) -> area

findInterections :: Vector -> Vector -> [(Vector, Vector)] -> [(Vector, Vector)]
findInterections a b polygonLines =
   filter (intesect (a, b)) polygonLines

intersectPolygon :: (Vector, Vector) -> [(Vector, Vector)] -> Bool
intersectPolygon line polygonLines =
  any (intesect line) polygonLines

intesect :: (Vector, Vector) -> (Vector, Vector) -> Bool
intesect ((ax, ay), (bx, by)) ((ex1, ey1), (ex2, ey2)) =
  if ex1 == ex2
    then ex1 `between` (ax, bx) && ((min ey1 ey2) `betweenStrict` (ay, by) || (max ey1 ey2) `betweenStrict` (ay, by))
    else ey1 `between` (ay, by) && ((min ex1 ex2) `betweenStrict` (ax, bx) || (max ex1 ex2) `betweenStrict` (ax, bx))
  where
    between v (a, b) =
        let a' = min a b
            b' = max a b
         in v > a' && v < b'
    betweenStrict v (a, b) =
        let a' = min a b
            b' = max a b
         in v >= a' && v <= b'

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
