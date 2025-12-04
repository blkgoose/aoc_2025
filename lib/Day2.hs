module Day2 (execute) where

import Data.List.Split (splitOn)
import Utils (trace, range)
import qualified Debug.Trace as Debug

execute :: String -> (Int, Int)
execute input =
    let groups = splitOn "," input
        allGroups = map (map read . splitOn "-") groups :: [[Int]]
        rangeIds = concatMap (\[start, end] -> range start end) allGroups

        part1 = sum $ filter isInvalid rangeIds
        part2 = sum $ filter isInvalid' rangeIds

    in (part1, part2)

isInvalid' :: Int -> Bool
isInvalid' id =
    let id' = show id
        halfId = take (length id' `div` 2) id'

    in something halfId id'


something :: [Char] -> String -> Bool
something [] _ = False
something block match =
    let repeatFor = length match `div` length block
        block' = concat $ replicate repeatFor block
    in
        if block' == match then
            True
        else
            something (init block) match

isInvalid :: Int -> Bool
isInvalid id =
    let digits = show id
        (left, right) = splitAt (length digits `div` 2) digits
    in left == right
