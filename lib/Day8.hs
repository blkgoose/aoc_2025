module Day8 (execute) where

import Data.List.Split (splitOn)
import Data.List (sortBy, nub, sortOn, groupBy, findIndex)
import Data.Ord (comparing)
import Data.Function (on)

import Flow

import Utils (traceList)

type Vector = (Int, Int, Int)

execute :: [String] -> (Int, Int)
execute input = 
    let vector = map vectorize input
        paired = 
            foldl (pairClosest vector) [] vector
                |> sortBy (comparing snd)
                |> traceList "Paired Vectors:"
                |> map fst
                |> take 11
        grouped = groupAllBy connected paired
        exploded = grouped
            |> map (nub . concatMap (\(a, b) -> [a, b]))
            |> sortBy (comparing length)
            |> reverse
            |> traceList "Grouped Vectors:"
            |> take 3
        counted = product $ map length exploded
    in (counted, 0)
    where
        vectorize :: String -> Vector
        vectorize line = 
            let line' = splitOn "," line
                x = read (line' !! 0) :: Int
                y = read (line' !! 1) :: Int
                z = read (line' !! 2) :: Int
             in (x, y, z)

connected :: (Vector, Vector) -> (Vector, Vector) -> Bool
connected (a1, b1) (a2, b2) =
    a1 == a2 || a1 == b2 || b1 == a2 || b1 == b2
                
groupAllBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupAllBy _ [] = []
groupAllBy pred list = go list []
    where
       go [] acc = acc
       go (el:rest) [] = go rest [[el]]
       go (el:rest) groups =
           let groupId = findIndex (any $ pred el) groups
           in case groupId of
                Just idx ->
                    let (before, g:after) = splitAt idx groups
                    in go rest (before ++ ((el:g):after))
                Nothing ->
                    go rest (groups ++ [[el]])

pairClosest :: [Vector] -> [((Vector, Vector), Double)] -> Vector -> [((Vector, Vector), Double)]
pairClosest vectors pairs v =
    let
        alreadyConnected = pairs
                |> map fst
                |> filter (\(_, c) -> c == v)
                |> map fst
                |> traceList ("Already connected to " ++ show v ++ ":")
        (closest: _) = vectors
            |> filter (/= v)
            |> filter (\vec -> not (vec `elem` alreadyConnected))
            |> traceList ("Available Vectors for "++ show v ++ ":")
            |> sortBy (comparing (distanceBetween v))
        distance = distanceBetween v closest
    in pairs ++ [((v, closest), distance)]

distanceBetween :: Vector -> Vector -> Double
distanceBetween (x1, y1, z1) (x2, y2, z2) =
    sqrt (fromIntegral ((x2 - x1) ^ 2 + (y2 - y1) ^ 2 + (z2 - z1) ^ 2))

distanceFromZero :: Vector -> Double
distanceFromZero = distanceBetween (0, 0, 0)
