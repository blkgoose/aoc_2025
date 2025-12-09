module Main (main) where

import System.Environment (getArgs)
import Control.Exception (catch)

import Day1
import Day2
import Day3
import Day6

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
        1 -> print $ Day1.execute input
        2 -> print $ Day2.execute input
        3 -> print $ Day3.execute input
        6 -> print $ Day6.execute input
        _   -> putStrLn "Day not implemented."

ignore :: IOError -> IO String
ignore _ = return ""
