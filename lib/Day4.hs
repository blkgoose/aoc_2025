module Day4 (execute) where

import Utils (groupWhile, trim, trace, range)
import Data.Char (isSpace)
import Data.List.Split (splitOn)

import Flow

execute :: [String] -> (Int, Int)
execute input =
    let [database', ingredients'] = groupWhile (not . null) input
        ingredients = map read ingredients' :: [Int]
        database = map (splitOn "-") database' |> map (\[a,b] -> (read a, read b)) :: [(Int, Int)]
    in ( filter (isInRange database) ingredients |> length
       , 0
       )

isInRange :: [(Int, Int)] -> Int -> Bool
isInRange database n =
    database
        |> any (\(a,b) -> n >= a && n <= b)
