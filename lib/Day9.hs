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
                        y >= min ay by && y <= max ay by
                    )
                    poly
                )
            )
          |> M.fromList
   in orderByLargestArea points
        |> find
          ( \(a, b, _) ->
              let rectangle = vertices a b
                  v = vertices a b
                    |> all
                      ( \(x, y) ->
                          let segments = cache ! y
                              left = segments
                                  |> filter (\((ax, ay), (bx, by)) ->
                                      if ay == by
                                      then if x == max ax bx then True
                                           else if x == min ax bx then False
                                           else x > min ax bx && x < max ax bx
                                      else ax <= x
                                  )
                                  |> length
                              right = segments
                                  |> filter (\((ax, ay), (bx, by)) ->
                                      if ay == by
                                      then
                                          if x == max ax bx then True
                                          else x > min ax bx && x < max ax bx
                                      else ax >= x
                                  )
                                  |> length
                           in not (even left && even right)
                      )

                  -- TODO: add all contained points inside the produced rectangle
                  allContainedPointsInRectangle =
                    [ (x, y)
                      | x <- [min (fst a) (fst b) .. max (fst a) (fst b)],
                        y <- [((min (snd a) (snd b)) + 1) .. ((max (snd a) (snd b)) - 1)]
                    ] |> any isPointInPolygon
                  isPointInPolygon p = any (== p) points
               in v && not allContainedPointsInRectangle
          )
        |> (\x -> case x of
              Just (a, b, area) -> area
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
