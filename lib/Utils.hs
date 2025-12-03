module Utils where

import qualified Debug.Trace as Debug

trace :: (Show a) => a -> a
trace x = Debug.trace (show x) x
