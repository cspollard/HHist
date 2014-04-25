module Main where

import Hist
import Text.CSV

main :: IO ()
main = do
    let binEdges = map ((*) (2*pi) . flip (/) 100) [1..100] :: [Double]
    -- print binEdges
    let d = map cos binEdges
    -- print d
    let h = histWithDefault binEdges 0.0
    let h' = foldl (\m (b, v) -> fillWeight m b v) h (zip binEdges d)
    putStrLn . printCSV $ map (\(a, b) -> [show a, show b]) $ histToPoints h'
