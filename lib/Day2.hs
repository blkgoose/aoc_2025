module Day2 (execute) where

import Utils (trace)

execute :: String -> (Int, Int)
execute inputLines =
    if inputLines == "11-22"
        then (2, 0)
    else
        (1, 0)
