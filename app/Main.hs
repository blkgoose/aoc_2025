module Main (main) where

import Day1

main :: IO ()
main = do
    contents <- readFile "app/day1.input"
    let inputLines = map read (lines contents)
    Day1.execute  inputLines
