module Day6 where

import Data.List (transpose)
import Data.Char (isSpace, isDigit)

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

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

groupWhile :: (String -> Bool) -> [String] -> [[String]]
groupWhile pred line = foldl (step) [] line
    where
        step [] x = [[x]]
        step (g:gs) x
            | pred x    = (g ++ [x]) : gs
            | otherwise = [] : (g:gs)

moveSymToFront :: [String] -> [String]
moveSymToFront (x:xs) =
    let sym = filter (not . isDigit) x |> trim
        rest = map (filter isDigit) (x:xs)
    in sym : rest
