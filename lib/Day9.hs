module Day9 (execute) where

import Data.List (find, singleton, sort, sortBy, sortOn)
import Data.List.Split (chunksOf, splitOn)
import Data.Map ((!))
import qualified Data.Map as M
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
    |> \((_, _, area) : _) -> area

part2 :: [Point] -> Int
part2 points =
  let poly = polygon points

      (minY, maxY) =
        points
          |> map snd
          |> (\ys -> (minimum ys, maximum ys))

      cache =
        [minY .. maxY]
          |> map
            ( \y ->
                ( y,
                  filter
                    ( \((ax, ay), (bx, by)) ->
                        if ax == bx then y > min ay by && y < max ay by -- strict, no vertical lines vertices
                        else y >= min ay by && y <= max ay by -- inclusive of horizontal lines
                    )
                    poly
                )
            )
          |> Utils.traceList "Cache: "
          |> M.fromList
   in orderByLargestArea points
        |> find
          ( \(a, b, _) ->
              vertices a b |> Utils.trace_ "Checking: "
                |> all
                  ( \(x, y) ->
                      let segOnLine = cache ! y
                          left = segOnLine |> filter (\((ax, _), (bx, _)) -> x < min ax bx) |> length |> Utils.trace_ ("Left at " ++ show (x, y) ++ ": ")
                          right = segOnLine |> filter (\((ax, _), (bx, _)) -> x > max ax bx) |> length |> Utils.trace_ ("Right at " ++ show (x, y) ++ ": ")
                          res = not (even left && even right)
                       in Utils.trace res
                  )
          )
        |> (\x -> case x of
              Just (a, b, area) -> Utils.trace' ("Found between " ++ show a ++ " and " ++ show b ++ " with area " ++ show area) area
              Nothing -> 0
           )
  where
    vertices :: Point -> Point -> [Point]
    vertices a b =
      [a, b, ((fst a, snd b)), ((fst b, snd a))]

polygon :: [Point] -> [(Point, Point)]
polygon points@(x : t) = zip points (t ++ [x])

orderByLargestArea :: [Point] -> [(Point, Point, Int)]
orderByLargestArea points =
  [(a, b, area a b) | a <- points, b <- points, a < b]
    |> sortBy (\(_, _, area1) (_, _, area2) -> compare area2 area1)

area :: Point -> Point -> Int
area (x1, y1) (x2, y2) =
   (abs(x1 - x2) + 1) * (abs(y1 - y2) + 1)
