module Day6 where

import Data.List (transpose)
import Utils (trace)
import Flow

execute :: [String] -> (Int, Int)
execute input =
    let part1 = input
                    |> map words . reverse
                    |> transpose
                    |> map processColumn
                    |> sum
        part2 = 0

    in (part1, part2)

processColumn :: [String] -> Int
processColumn ("*":values) =
    foldl (*) 1 (map read values)
processColumn ("+":values) =
    foldl (+) 0 (map read values)
