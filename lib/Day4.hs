module Day4 (execute) where

import Data.Maybe (mapMaybe)
import Data.Array

import Utils (Coord, Grid, makeGrid)

import Flow

data Spot = Roll | Space deriving (Eq, Show)

execute :: [String] -> (Int, Int)
execute input =
    let grid = makeGrid $ map (map parseSpot) input
    in (part1 grid, part2 grid)

part1 :: Grid Spot -> Int
part1 grid =
    let (finalGrid, changes) = step grid
    in changes

part2 :: Grid Spot -> Int
part2 grid = go grid 0
  where
    go g acc =
      let (g', changes) = step g
      in if changes == 0
         then acc
         else go g' (acc + changes)

adjacents :: Coord -> Grid Spot -> [Spot]
adjacents (x, y) grid =
    [ grid ! (nx, ny)
    | dx <- [-1..1], dy <- [-1..1], (dx,dy) /= (0,0)
    , let nx = x + dx
    , let ny = y + dy
    , inRange (bounds grid) (nx, ny)
    ]

isSpotGood :: Grid Spot -> Coord -> Bool
isSpotGood grid coord =
    case grid ! coord of
        Space -> False
        Roll  ->
          let rollCount = length $ filter (== Roll) (adjacents coord grid)
          in rollCount < 4

step :: Grid Spot -> (Grid Spot, Int)
step grid =
    let changes =
            [ coord
            | (coord, Roll) <- assocs grid
            , isSpotGood grid coord
            ]
        newGrid = grid // [ (c, Space) | c <- changes ]
    in (newGrid, length changes)

parseSpot :: Char -> Spot
parseSpot '@' = Roll
parseSpot '.' = Space
