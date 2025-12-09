module Day5 (execute) where

import Utils (groupWhile)
import Data.Char (isSpace)
import Data.List.Split (splitOn)
import Data.List (sortBy)

import Flow

execute :: [String] -> (Int, Int)
execute input =
    let [database', ingredients'] = groupWhile (not . null) input
        ingredients = map read ingredients' :: [Int]
        database = processDatabase database'
    in ( part1 database ingredients
       , part2 database
       )

part1 :: [(Int, Int)] -> [Int] -> Int
part1 database ingredients =
    filter (isInRange database) ingredients
        |> length

part2 :: [(Int, Int)] -> Int
part2 database =
    map (\(a,b) -> b - a + 1) database
        |> sum

processDatabase :: [String] -> [(Int, Int)]
processDatabase database =
    map (splitOn "-") database
        |> map (\[a,b] -> (read a, read b))
        |> sortBy (\(a1,_) (a2,_) -> compare a1 a2)
        |> simplifyRanges
    where
        simplifyRanges :: [(Int, Int)] -> [(Int, Int)]
        simplifyRanges [] = []
        simplifyRanges [x] = [x]
        simplifyRanges ((a1,b1):(a2,b2):xs)
            | b1 >= a2  = simplifyRanges ((a1, max b1 b2):xs)
            | otherwise = (a1,b1) : simplifyRanges ((a2,b2):xs)


isInRange :: [(Int, Int)] -> Int -> Bool
isInRange database n =
    database
        |> any (\(a,b) -> n >= a && n <= b)
