-- {-# LANGUAGE FlexibleInstances #-}

module Hist where

import Control.Arrow (first)
import qualified Data.Map as M
import Bin
import Uncertain

type Hist b v = M.Map (Bin b) v
type Hist2D b v = M.Map (Bin b, Bin b) v

histWithDefault :: Ord b => [Bin b] -> v -> Hist b v
histWithDefault bins def = M.fromList $ zip bins (repeat def)

fillWeight :: (Ord b, Num v) => Hist b v -> b -> v -> Hist b v
fillWeight h x w = M.adjust (w +) (Val x) h

fill :: (Ord b, Num v) => Hist b v -> b -> Hist b v
fill h x = fillWeight h x 1

histToPoints :: (Fractional b) => Hist b v -> [(b, v)]
histToPoints h = map (first midPoint) $ M.toList h

mapBins :: Ord c => (b -> c) -> Hist b v -> Hist c v
mapBins f = M.mapKeys (fmap f)

mapContents :: (v -> w) -> Hist b v -> Hist b w
mapContents = M.map

fromLists :: (Ord b) => [b] -> [v] -> Hist b v
fromLists bs vs = M.fromList $ zip (fromLowEdges bs) vs

toList :: Hist b v -> [(Bin b, v)]
toList = M.toList

-- generally we want Doubles everywhere and to start with zeros.
th1d :: [Double] -> Hist Double (U Double)
th1d bs = histWithDefault (fromLowEdges bs) 0.0

{-
-- TODO
th2d :: [Double] -> [Double] -> Hist2D Double (U Double)
th2d xs ys = histWithDefault (fromLowEdges xs) $ th1d ys
-}
