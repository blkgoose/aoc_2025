module Main (main) where

import Control.Exception (catch)
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Flow
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (day : _) -> runDay (read day)
    _ -> mapM_ runDay [1 .. 12]

runDay :: Int -> IO ()
runDay day = do
  content <- catch (readFile $ "app/day" ++ show day ++ ".input") ignore
  let input = lines content

  putStrLn $ "\n=== Day " ++ show day ++ " ==="
  let run = case day of
        1 -> Just . Day1.execute
        2 -> Just . Day2.execute
        3 -> Just . Day3.execute
        4 -> Just . Day4.execute
        5 -> Just . Day5.execute
        6 -> Just . Day6.execute
        7 -> Just . Day7.execute
        _ -> \_ -> Nothing

  run input
    |> maybe ("No implementation for this day.") showResult
    |> putStrLn

showResult :: (Show a, Show b) => (a, b) -> String
showResult (part1, part2) =
  "Part 1: " ++ show part1 ++ "\nPart 2: " ++ show part2

ignore :: IOError -> IO String
ignore _ = return ""
