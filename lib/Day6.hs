module Day6 where

import Data.List (transpose)
import Data.Char (isSpace)
import Utils (trace)

execute :: [String] -> (Int, Int)
execute input =
    let part1 = processColumn (trace $ reverse (map strip input))
        part2 = 0

    in (part1, part2)

processColumn :: [String] -> Int
processColumn ("*":values) =
    foldl ((*)) 1 (map read values)


strip :: String -> String
strip = f . f
   where f = reverse . dropWhile isSpace
