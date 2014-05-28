{-# LANGUAGE GADTs #-}

module Hist where

import Data.Monoid ((<>))


-- histograms are parameterized by the binning parameter (b) and the
-- content parameter (v)
class Hist h where
    extractH :: Ord b => h b v -> b -> v
    fillH :: (Ord b, Num v) => h b v -> b -> v -> h b v

class Bin b where
    containsB :: Ord a => b a v -> a -> Bool
    fillB :: Num v => b a v -> v -> b a v


data Interval a = Interval a a deriving Show

instance Eq a => Eq (Interval a) where
    (Interval w x) == (Interval y z) = w == y && x == z

instance Ord a => Ord (Interval a) where
    (Interval w x) `compare` (Interval y z) = w `compare` y <> x `compare` z


data HistBin b v = HistBin {
    interval :: Interval b,
    value :: v
    } deriving (Show, Ord, Eq)

instance Bin HistBin where
    containsB (HistBin (Interval xmin xmax) _) x = xmin <= x && x < xmax

    fillB (HistBin b w) wgt = HistBin b (w+wgt)


data Histogram b v = Histogram [HistBin b v]

fromListWithDefault :: Ord b => v -> [b] -> [HistBin b v]
fromListWithDefault _ [] = []
fromListWithDefault _ [_] = []
fromListWithDefault def (x:x':xs) = HistBin (Interval x x') def : fromListWithDefault def (x':xs)

fillBIfContains :: (Bin b, Ord a, Num v) => [b a v] -> a -> v -> [b a v]
fillBIfContains (b:bs) x wgt = if b `containsB` x
                            then b `fillB` wgt : bs
                            else b : fillBIfContains bs x wgt
fillBIfContains [] _ _ = []


instance Hist Histogram where
    fillH (Histogram bs) x wgt = Histogram $ fillBIfContains bs x wgt

    extractH (Histogram (b:bs)) x = if b `containsB` x
                                        then value b
                                        else extractH (Histogram bs) x

    extractH (Histogram []) _ = error "no bin containing value found in histogram."
