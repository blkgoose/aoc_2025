module Day9 (execute) where

import Data.List (singleton, sortBy, sortOn, sort)
import Data.List.Split (splitOn, chunksOf)
import qualified Data.HashSet as HS
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
      filledPoly = fillPolygon poly
   in orderByLargestArea points
    |> filter (\(a, b, _) -> [a, b] |> all (`HS.member` filledPoly))
    |> \((_, _, area):_) -> area

fillPolygon :: [(Point, Point)] -> HS.HashSet Point
fillPolygon edges =
  let filledEdges = HS.fromList $ concatMap fillEdge edges

      points = map fst edges
      minX = minimum $ map fst points
      maxX = maximum $ map fst points
      minY = minimum $ map snd points
      maxY = maximum $ map snd points

      allPoints = [(x, y) | x <- [minX .. maxX], y <- [minY .. maxY]] |> Utils.traceList "All points:"

      filled = allPoints
        |> foldl (accInsidePoints filledEdges) ([], 0)
        |> fst
        |> HS.fromList
      filledAndNotFilled = allPoints
          |> map (\p ->
              if p `HS.member` filledEdges then "E"
              else if p `HS.member` filled then "#"
              else " "
          )
          |> chunksOf (maxY - minY + 1)
          |> Utils.traceList "Filled and not filled points:"
   in Utils.trace' filledAndNotFilled filled
  where
    accInsidePoints :: HS.HashSet Point -> ([Point], Int) -> Point -> ([Point], Int)
    accInsidePoints edges (set, edgeCount) point =
      let onEdge = HS.member point edges
          wasRidingTheEdge = edgeCount > 1
          isInside = odd edgeCount
      in case (isInside, onEdge) of
        (False, True)  -> (point:set, edgeCount + 1)
        (True, True)    -> (point:set, edgeCount)
        (False, False)  -> (set, edgeCount)
        (True, False)   -> (point:set, edgeCount)
        _ -> (set, edgeCount)

    fillEdge :: (Point, Point) -> [Point]
    fillEdge ((x1, y1), (x2, y2)) =
      let minX = min x1 x2
          maxX = max x1 x2
          minY = min y1 y2
          maxY = max y1 y2
       in [(x, y) | x <- [minX .. maxX], y <- [minY .. maxY]]


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
