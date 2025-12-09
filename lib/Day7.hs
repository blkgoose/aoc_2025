module Day7 (execute) where

import Utils (makeGrid, Coord, Grid)
import Data.Array

import Flow

import Utils (trace, trace')

data Spot = Splitter
          | TriggeredSplitter
          | Space
          | Generator
          | Beam
      deriving (Eq, Show)

execute :: [String] -> (Int, Int)
execute input =
    let grid = makeGrid $ map (map parseSpot) input
    in (part1 grid, 0)

part1 :: Grid Spot -> Int
part1 grid = go grid
        |> elems
        |> filter ((==) TriggeredSplitter)
        |> length
    where
        go :: Grid Spot -> Grid Spot
        go state =
            let state' = step state
            in if state' == state
                then state
                else go state'

step :: Grid Spot -> Grid Spot
step grid =
    array (bounds grid) [ (ix, stepCell grid ix) | ix <- indices grid ]

stepCell :: Grid Spot -> Coord -> Spot
stepCell grid (0,c) = grid ! (0,c)
stepCell grid (r,c) =
    let me = grid ! (r,c)
        above = grid ! (r-1,c)
    in case (me, above) of
        (Space, Generator)       -> Beam
        (Splitter, Beam)         -> TriggeredSplitter
        (Space, Beam)            -> Beam
        _                        -> me

parseSpot :: Char -> Spot
parseSpot 'S' = Generator
parseSpot '^' = Splitter
parseSpot '.' = Space

renderGrid :: Grid Spot -> [String]
renderGrid grid =
    [ [ renderSpot (grid ! (x, y)) | y <- [yMin .. yMax] ]
        | x <- [xMin .. xMax]
        ]
    where
        renderSpot Splitter  = '^'
        renderSpot Generator = 'S'
        renderSpot Space     = '.'
        renderSpot Beam      = '|'

        (xMin, yMin) = fst (bounds grid)
        (xMax, yMax) = snd (bounds grid)
        width = yMax - yMin + 1
        height = xMax - xMin + 1
