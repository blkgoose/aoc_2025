module Day1 (execute) where

import Utils (trace, range)

execute :: [String] -> (Int, Int)
execute inputLines = do
    let input = map parseStep inputLines
        start = (50, 0)
        onlyStoppingOn0 = snd $ foldl process start input
        passingThrough0 = snd $ foldl process' start input
    (onlyStoppingOn0, passingThrough0)

process :: (Int, Int) -> Int -> (Int, Int)
process (position, count) step = do
    let newPosition = (position + step) `mod` 100
        counter = if newPosition == 0 then count + 1 else count

    (newPosition, counter)

process' :: (Int, Int) -> Int -> (Int, Int)
process' (position, count) step = do
    let allPositions' = drop 1 $ range position (position + step)
        allPositions = map (`mod` 100) allPositions'
        counter = length . filter (== 0) $ allPositions

    (last allPositions, count + counter)

parseStep :: String -> Int
parseStep (dir:steps)
    | dir == 'L' = -read steps
    | dir == 'R' = read steps
    | otherwise = 0
