module Day8 (execute) where

import Data.List.Split (splitOn)
import Data.List (sortBy, nub, sortOn, groupBy, findIndex, sort)
import Data.Ord (comparing)
import Data.Function (on)

import Flow

import Utils (traceList)

type Vector = (Int, Int, Int)

execute :: Int -> [String] -> (Int, Int)
execute span input = 
    let vector = map vectorize input
        paired = [(a, b, distanceBetween a b) | a <- vector, b <- vector, a < b]
                |> sortOn (\(_, _, d) -> d)
                |> map (\(a, b, _) -> (a, b))
        sets = [[x] | x <- vector]
    in (part1 span paired sets, 0)
    where
        vectorize :: String -> Vector
        vectorize line = 
            let line' = splitOn "," line
                x = read (line' !! 0) :: Int
                y = read (line' !! 1) :: Int
                z = read (line' !! 2) :: Int
             in (x, y, z)

part1 :: Int -> [(Vector, Vector)] -> [[Vector]] -> Int
part1 span pairs sets =
    let paired = take span pairs
        groups = foldl union sets paired
        top3 = map length groups
            |> sort
            |> reverse
            |> take 3
            |> product
    in top3

union :: [[Vector]] -> (Vector, Vector) -> [[Vector]]
union sets (a, b) =
    let seta = findSet sets a
        setb = findSet sets b
    in if seta == setb || null seta || null setb
       then sets
       else [s | s <- sets, s /= seta, s /= setb] ++ [seta ++ setb]

findSet :: [[Vector]] -> Vector -> [Vector]
findSet sets v =
    case [s | s <- sets, v `elem` s] of
        []    -> []
        (s:_) -> s

connected :: (Vector, Vector) -> (Vector, Vector) -> Bool
connected (a1, b1) (a2, b2) =
    a1 == a2 || a1 == b2 || b1 == a2 || b1 == b2
                
distanceBetween :: Vector -> Vector -> Double
distanceBetween (x1, y1, z1) (x2, y2, z2) =
    sqrt (fromIntegral ((x2 - x1) ^ 2 + (y2 - y1) ^ 2 + (z2 - z1) ^ 2))
