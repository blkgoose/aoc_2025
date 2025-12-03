module Day1 (execute) where

execute :: [String] -> Int
execute inputLines =
    snd $ foldl process (50, 0) $ map parseStep inputLines

process :: (Int, Int) -> Int -> (Int, Int)
process (position, count) step = do
    let newPosition = (position + step) `mod` 100
    let counter = if newPosition == 0 then count + 1 else count

    (newPosition, counter)

parseStep :: String -> Int
parseStep (dir:steps)
    | dir == 'L' = -read steps
    | dir == 'R' = read steps
    | otherwise = 0
