module Main (main) where

import System.Environment (getArgs)
import Control.Exception (catch)

import Day1
import Day2

main :: IO ()
main = do
    args <- getArgs
    case args of
        (day:_) -> do 
            content <- catch (readFile $ "app/day" ++ day ++ ".input") ignore
            let input = lines content

            case day of
                "1" -> day1 input
                "2" -> day2 input
                _   -> putStrLn "Day not implemented."
        _     -> putStrLn "Wrong number of arguments. Usage: run <day>"


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
