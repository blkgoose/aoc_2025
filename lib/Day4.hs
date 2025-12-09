module Day4 (execute) where

import Utils (groupWhile, trim, trace, range)
import Data.Char (isSpace)
import Data.List.Split (splitOn)

import Debug.Trace as Debug

import Flow

execute :: [String] -> (Int, Int)
execute input =
    let [database', ingredients'] = groupWhile (not . null) input
        ingredients = map read ingredients' :: [Int]
        database = concat $ map explode database' :: [Int]
    in ( filter (`elem` database) ingredients |> length
       , 0
       )

explode :: String -> [Int]
explode line =
    let [start, end] = splitOn "-" line
    in range (read start) (read end)
