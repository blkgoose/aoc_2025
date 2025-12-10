module Day2 (execute) where

import Data.List.Split (splitOn)
import Utils (trace, range)
import qualified Debug.Trace as Debug

execute :: [String] -> (Int, Int)
execute (input:_) =
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

    in isInternallyRepeating halfId id'


isInternallyRepeating :: [Char] -> String -> Bool
isInternallyRepeating [] _ = False
isInternallyRepeating block match =
    let repeatFor = length match `div` length block
        block' = concat $ replicate repeatFor block
    in
        if block' == match then
            True
        else
            isInternallyRepeating (init block) match

isInvalid :: Int -> Bool
isInvalid id =
    let digits = show id
        (left, right) = splitAt (length digits `div` 2) digits
    in left == right
