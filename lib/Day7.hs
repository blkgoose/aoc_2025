module Day7 (execute) where

import Utils (makeGrid, Coord, Grid)
import Data.Array
import Data.Maybe (fromMaybe)

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
        left = fromMaybe Space $ safeGet (r,c-1) grid
        right = fromMaybe Space $ safeGet (r,c+1) grid
        above = grid ! (r-1,c)
    in case ([left, me, right], above) of
        ([_, Space,                 _], Generator) -> Beam
        ([_, Splitter,              _], Beam)      -> TriggeredSplitter
        ([_, Space,                 _], Beam)      -> Beam
        ([TriggeredSplitter, Space, _], _)         -> Beam
        ([_, Space, TriggeredSplitter], _)         -> Beam
        _                             -> me
    where
        safeGet :: Coord -> Grid Spot -> Maybe Spot
        safeGet idx arr =
          if inRange (bounds arr) idx
            then Just (arr ! idx)
            else Nothing

parseSpot :: Char -> Spot
parseSpot 'S' = Generator
parseSpot '^' = Splitter
parseSpot '.' = Space
