module Day3 where

import Data.Char (digitToInt)

import Flow

execute :: [String] -> (Int, Int)
execute input =
    ( input
        |> map (processLine 2)
        |> sum
    , input
        |> map (processLine 12)
        |> sum
    )

processLine :: Int -> [Char] -> Int
processLine buffenLen line =
    let input = map digitToInt line
        buffer = take buffenLen input
        battery = drop buffenLen input
    in process buffer battery

process :: [Int] -> [Int] -> Int
process acc [] = foldl ((+) . (*10)) 0 acc
process acc (x:xs) =
    process (reduce $ acc ++ [x]) xs

reduce :: Ord a => [a] -> [a]
reduce (x:y:xs)
    | y > x     = y : xs
    | otherwise = x : reduce (y:xs)
reduce _ = []
