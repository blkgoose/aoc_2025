module Day3 where

import Flow
import Utils (trace)

execute :: [String] -> (Int, Int)
execute input =
    ( map processLine input
          |> sum
    , 0
    )

processLine :: [Char] -> Int
processLine line = 
    line
        |> map (\c -> read [c] :: Int)
        |> process []

process :: [Int] -> [Int] -> Int
process acc [] = foldl ((+) . (*10)) 0 acc
process [] (x:xs) = process [x] xs
process [a] (x:xs) = process [a, x] xs
process [a, b] (x:xs) =
    if b > a then
        process [b, x] xs

    else if x > b then
        process [a, x] xs

    else
        process [a, b] xs

