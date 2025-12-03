module Main (main) where

import System.Environment (getArgs)
import Control.Exception (catch)

import Day1

main :: IO ()
main = do
    args <- getArgs
    case args of
        (day:_) -> do 
            content <- catch (readFile $ "app/day" ++ day ++ ".input") ignore
            input <- return $ lines content

            case day of
                "1" -> day1 input
                _   -> putStrLn "Day not implemented."
        _     -> putStrLn "Wrong number of arguments. Usage: run <day>"


ignore :: IOError -> IO String
ignore _ = return ""

day1 :: [String] -> IO ()
day1 input = do
    print $ Day1.execute input
