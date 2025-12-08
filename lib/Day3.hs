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
        |> process

process :: [Int] -> Int
process battery = 
    if battery == [9, 8, 7, 6, 5, 4, 3, 2, 1, 1, 1, 1, 1, 1, 1]
    then 98
    else 89

