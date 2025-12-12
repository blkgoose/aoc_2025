module Day9 (execute) where

import Data.List (singleton, sortBy, sortOn)
import Data.List.Split (splitOn)
import Flow
import Utils

type Point = (Int, Int)

execute :: [String] -> (Int, Int)
execute input =
  let points = map parse input
   in (part1 points, part2 points)
  where
    parse :: String -> Point
    parse line =
      let line' = splitOn "," line
          x = read (line' !! 0) :: Int
          y = read (line' !! 1) :: Int
       in (x, y)

part1 :: [Point] -> Int
part1 points =
  orderByLargestArea points
    |> \((_, _, area):_) -> area

part2 :: [Point] -> Int
part2 points =
  let poly = polygon points
   in orderByLargestArea points
    |> filter (\(a, b, _) -> not $ intersectPolygon (a, b) poly)
    |> filter (\(a, b, _) -> not $ any (isPointInPolygon poly) [a, b])
    |> \((_, _, area):_) -> area

isPointInPolygon :: [(Point, Point)] -> Point -> Bool
isPointInPolygon poly (px, py) =
    let rays = [ ((px, py), (maxX + 1, py))
               , ((px, py), (minX - 1, py))
               , ((px, py), (px, maxY + 1))
               , ((px, py), (px, minY - 1))
               ]
        maxX = maximum $ map (fst . fst) poly
        minX = minimum $ map (fst . fst) poly
        maxY = maximum $ map (snd . fst) poly
        minY = minimum $ map (snd . fst) poly
     in map (\ray -> countIntersections ray poly) rays
        |> all odd
     where
       countIntersections ray polyLines =
         length $ filter (intesect ray) polyLines

intersectPolygon :: (Point, Point) -> [(Point, Point)] -> Bool
intersectPolygon line polygonLines =
  any (intesect line) polygonLines

intesect :: (Point, Point) -> (Point, Point) -> Bool
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

polygon :: [Point] -> [(Point, Point)]
polygon points@(x:t) = zip points (t ++ [x])

orderByLargestArea :: [Point] -> [(Point, Point, Int)]
orderByLargestArea points =
  [(a, b, area a b) | a <- points, b <- points, a < b]
    |> sortBy (\(_, _, area1) (_, _, area2) -> compare area2 area1)

area :: Point -> Point -> Int
area (x1, y1) (x2, y2) =
  let maxX = max x1 x2
      minX = min x1 x2
      maxY = max y1 y2
      minY = min y1 y2
   in (maxX - minX + 1) * (maxY - minY + 1)
