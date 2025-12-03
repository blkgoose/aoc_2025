module Day2 (execute) where

import Data.List.Split (splitOn)
import Utils (trace, range)

execute :: String -> (Int, Int)
execute input = do
    let groups = splitOn "," input
        allGroups = map (map read . splitOn "-") groups :: [[Int]]

        rangeIds = concatMap (\r ->
                       let (start, end) = makeTuple r
                       in range start end
                   ) allGroups

    (sum $ filter isInvalid rangeIds, 0)

makeTuple :: [Int] -> (Int, Int)
makeTuple [a, b] = (a, b)

isInvalid :: Int -> Bool
isInvalid id = 
    let digits = show id
        (left, right) = splitAt (length digits `div` 2) digits
    in left == right
