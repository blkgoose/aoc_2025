module Day4 (execute) where

import Utils (trace', trace)
import Data.Maybe (mapMaybe)

import Flow

type Grid = [[Spot]]
data Spot = Roll | Space deriving (Eq, Show)

execute :: [String] -> (Int, Int)
execute input =
    let grid = map (map rollOrSpace) input
    in (part1 grid, 0)

part1 :: Grid -> Int
part1 grid' =
    let grid = zipMatrix grid'
    in grid
        |> concatMap (map isSpotGood)
        |> filter id
        |> length
    where
        isSpotGood (_, Space) = False
        isSpotGood ((x, y), Roll) =
            let neighbors = adjacents x y grid'
                rollCount = length $ filter (== Roll) neighbors
            in rollCount < 4

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
  | i >= 0 && i < length xs = Just (xs !! i)
  | otherwise               = Nothing

safeGet :: Int -> Int -> Grid -> Maybe Spot
safeGet r c mat = do
  row <- safeIndex mat r
  safeIndex row c

adjacents :: Int -> Int -> Grid -> [Spot]
adjacents r c mat = mapMaybe (\(r',c') -> safeGet r' c' mat) neighbors
  where
    neighbors = [ (r-1, c-1), (r-1, c), (r-1, c+1)
                , (r,   c-1),           (r,   c+1)
                , (r+1, c-1), (r+1, c), (r+1, c+1)
                ]

zipMatrix :: [[a]] -> [[((Int, Int), a)]]
zipMatrix m =
  [ [ ((rowIdx, colIdx), val)
      | (colIdx, val) <- zip [0..] row ]
    | (rowIdx, row) <- zip [0..] m ]

rollOrSpace :: Char -> Spot
rollOrSpace '.' = Space
rollOrSpace '@' = Roll
