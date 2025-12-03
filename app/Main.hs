module Main (main) where

import System.Environment (getArgs)

import Day1

main :: IO ()
main = do
    args <- getArgs
    case args of
        (day:_) -> do 
            input <- lines <$> readFile ("app/day" ++ day ++ ".input")
            case day of
                "1" -> day1 input
                _   -> putStrLn "Day not implemented."
        _     -> putStrLn "Wrong number of arguments. Usage: run <day>"


day1 :: [String] -> IO ()
day1 input = do
    print $ Day1.execute input
