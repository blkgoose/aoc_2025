module Main (main) where

import System.Environment (getArgs)
import Control.Exception (catch)

import Day1
import Day2

main :: IO ()
main = do
    args <- getArgs
    case args of
        (day:_) -> runDay (read day)
        _       -> mapM_ runDay [1 .. 12]

runDay :: Int -> IO ()
runDay day = do
    content <- catch (readFile $ "app/day" ++ show day ++ ".input") ignore
    let input = lines content

    putStrLn $ "\n=== Day " ++ show day ++ " ==="
    case day of
        1 -> day1 input
        2 -> day2 input
        _   -> putStrLn "Day not implemented."

ignore :: IOError -> IO String
ignore _ = return ""

day1 :: [String] -> IO ()
day1 input = do
    print $ Day1.execute input

day2 :: [String] -> IO ()
day2 input
    | [x] <- input = do
        print $ Day2.execute x
    | otherwise = putStrLn "Invalid input for day 2."
