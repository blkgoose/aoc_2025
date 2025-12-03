module Main (main) where

import Day1

main :: IO ()
main = do
    contents <- readFile "app/day1.input"
    let inputLines = lines contents :: [String]
    print $ Day1.execute inputLines
