module Day3 where

import Data.Char (digitToInt)

import Flow
import Utils (trace)

execute :: [String] -> (Int, Int)
execute input =
    ( map (processLine 2) input
        |> sum
    , 0
    )

processLine :: Int -> [Char] -> Int
processLine buffenLen line =
    let input = map digitToInt line
        buffer = take buffenLen input
        battery = drop buffenLen input
    in process buffer battery

process :: [Int] -> [Int] -> Int
process acc [] = foldl ((+) . (*10)) 0 acc
process [a, b] (x:xs) =
    if b > a then
        process [b, x] xs

    else if x > b then
        process [a, x] xs

    else
        process [a, b] xs

