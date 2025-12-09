module Day6 where

import Data.List (transpose)
import Utils (trace)
import Data.Char (isSpace)
import Data.List.Split (splitOn, chunksOf)
import Flow

execute :: [String] -> (Int, Int)
execute input =
    let part1 = input
                    |> map words . reverse
                    |> transpose
                    |> map processColumn
                    |> sum
        part2 = input
                    |> map (smartSplit " ")
                    |> transpose
                    |> map reverse
                    |> map processColumn'
                    |> sum

    in (part1, part2)

processColumn' :: [String] -> Int
processColumn' (sym:values) =
    let v = values
                |> reverse
                |> transpose
                |> map trim
                |> map read
    in if trim sym == "*" then
           foldl (*) 1 v
       else
           sum v

processColumn :: [String] -> Int
processColumn ("*":values) =
    foldl (*) 1 (map read values)
processColumn ("+":values) =
    sum (map read values)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

smartSplit :: String -> String -> [String]
smartSplit delim str =
    let parts = splitOn delim str
        maxLen = 3
    in map (take maxLen) $ chunksOf (maxLen + 1) str
