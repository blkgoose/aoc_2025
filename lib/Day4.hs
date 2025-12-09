module Day4 (execute) where

import Utils (groupWhile, trim, trace)
import Data.Char (isSpace)

execute :: [String] -> (Int, Int)
execute input =
    let [database, ingredients] = groupWhile (not . null) input
    in if elem "2" (trace $ ingredients)
         then (1, 0)
         else (0, 0)
