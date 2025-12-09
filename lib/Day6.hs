module Day6 where

import Data.List (transpose)
import Data.Char (isDigit)
import Utils (groupWhile, trim)

import Flow

execute :: [String] -> (Int, Int)
execute input =
    let part1 = input
                    |> map words . reverse
                    |> transpose
                    |> map processColumn
                    |> sum
        part2 = input
                    |> transpose
                    |> groupWhile (\l -> trim l /= "")
                    |> map moveSymToFront
                    |> map processColumn
                    |> sum

    in (part1, part2)

processColumn :: [String] -> Int
processColumn ("*":values) =
    foldl (*) 1 (map read values)
processColumn ("+":values) =
    sum (map read values)

moveSymToFront :: [String] -> [String]
moveSymToFront (x:xs) =
    let sym = filter (not . isDigit) x |> trim
        rest = map (filter isDigit) (x:xs)
    in sym : rest
