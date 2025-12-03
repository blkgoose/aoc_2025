module Day2 (execute) where

import Data.List.Split (splitOn)
import Utils (trace, range)

execute :: String -> (Int, Int)
execute inputLine = do
    let [from, to] = map read $ splitOn "-" inputLine
        rangeIds = trace $ range from to

    (sum $ filter isInvalid rangeIds, 0)

isInvalid :: Int -> Bool
isInvalid id
   | id == 11 = True
   | id == 22 = True
   | id == 33 = True
   | otherwise = False
