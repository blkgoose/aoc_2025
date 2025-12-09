module Day4 (execute) where

import Data.Maybe (mapMaybe)

import Flow

type Grid = [[Spot]]
data Spot = Roll | Space deriving (Eq, Show)

execute :: [String] -> (Int, Int)
execute input =
    let grid = map (map rollOrSpace) input
    in (part1 grid, part2 grid)

part1 :: Grid -> Int
part1 grid' =
    let grid = zipMatrix grid'
    in grid
        |> concatMap (map (isSpotGood grid'))
        |> filter id
        |> length

isSpotGood :: Grid -> ((Int, Int), Spot) -> Bool
isSpotGood _ (_, Space) = False
isSpotGood grid ((x, y), Roll) =
    let neighbors = adjacents x y grid
        rollCount = length $ filter (== Roll) neighbors
    in rollCount < 4

part2 :: Grid -> Int
part2 grid =
    until isDelta0 process (grid, 0, -1)
        |> \(_, count, _) -> count
    where 
        isDelta0 :: (Grid, Int, Int) -> Bool
        isDelta0 (_, _, 0) = True
        isDelta0 _         = False

        process :: (Grid, Int, Int) -> (Grid, Int, Int)
        process (grid, count, _) =
            let (newGrid, changes) = step grid
            in (newGrid, count + changes, changes)

        step :: Grid -> (Grid, Int)
        step grid =
            let m = zipMatrix grid
                r = map (map (countAndUpdate grid)) m :: [[ (Spot, Int) ]]
                newGrid = map (map fst) r
                changes = sum $ map (sum . map snd) r
            in (newGrid, changes)

        countAndUpdate :: Grid -> ((Int, Int), Spot) -> (Spot, Int)
        countAndUpdate g v@(_, s)
            | isSpotGood g v && s == Roll = (Space, 1)
            | s == Space                  = (Space, 0)
            | otherwise                   = (Roll, 0)

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
