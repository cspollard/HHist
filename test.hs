module Main where

import Data.Hist
import Data.Uncertain

main :: IO ()
main = do
    print . fmap pois $ fromAscList [1.0, 2.0] [0.0, 3.0, 6.0]
