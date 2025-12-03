module Day1 (execute) where

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
    let newPosition = (position + step)
        counter = if (newPosition >= 100 || newPosition <= 0 && position /= 0)
            then count + 1
            else count

    (newPosition `mod` 100, counter)

parseStep :: String -> Int
parseStep (dir:steps)
    | dir == 'L' = -read steps
    | dir == 'R' = read steps
    | otherwise = 0
