module Day7 (execute) where

import Data.Array
import Data.Maybe (fromMaybe)
import Flow
import Utils (Coord, Grid, makeGrid)

data Spot
  = Splitter
  | TriggeredSplitter Int
  | Space
  | Generator
  | Beam Int
  deriving (Eq, Show)

execute :: [String] -> (Int, Int)
execute input =
  let grid =
        input
          |> map (map parseSpot)
          |> makeGrid
          |> simulate
   in (part1 grid, part2 grid)

part1 :: Grid Spot -> Int
part1 grid =
  grid
    |> elems
    |> filter (\s -> case s of TriggeredSplitter _ -> True; _ -> False)
    |> length

part2 :: Grid Spot -> Int
part2 grid =
  let ((_, colMin), (rowMax, colMax)) = bounds grid
      lastRow = [grid ! (rowMax, col) | col <- [colMin .. colMax]]
   in filter isBeam lastRow
        |> foldl (\acc (Beam n) -> acc + n) 0
  where
    isBeam :: Spot -> Bool
    isBeam (Beam _) = True
    isBeam _ = False

simulate :: Grid Spot -> Grid Spot
simulate state =
  let state' = step state
   in if state' == state
        then state
        else simulate state'

step :: Grid Spot -> Grid Spot
step grid =
  array (bounds grid) [(ix, stepCell grid ix) | ix <- indices grid]

stepCell :: Grid Spot -> Coord -> Spot
stepCell grid (0, c) = grid ! (0, c)
stepCell grid (r, c) =
  let me = grid ! (r, c)
      left = fromMaybe Space $ safeGet (r, c - 1) grid
      right = fromMaybe Space $ safeGet (r, c + 1) grid
      above = grid ! (r - 1, c)
   in case ([left, me, right], above) of
        -- combining beams and splitters
        ([TriggeredSplitter n, _, TriggeredSplitter m], Space) -> Beam (n + m)
        ([TriggeredSplitter n, _, TriggeredSplitter m], Beam b) -> Beam (n + m + b)
        ([_, _, TriggeredSplitter m], Beam b) -> Beam (m + b)
        ([_, _, TriggeredSplitter m], Space) -> Beam m
        ([TriggeredSplitter m, _, _], Beam b) -> Beam (m + b)
        ([TriggeredSplitter m, _, _], Space) -> Beam m
        ([_, TriggeredSplitter _, _], Beam n) -> TriggeredSplitter n
        ([_, Beam _, _], Beam n) -> Beam n
        -- base rules
        ([_, Space, _], Generator) -> Beam 1
        ([_, Splitter, _], Beam n) -> TriggeredSplitter n
        ([_, Space, _], Beam n) -> Beam n
        _ -> me
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
