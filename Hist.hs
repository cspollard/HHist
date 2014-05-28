-- {-# LANGUAGE GADTs, TypeSynonymInstances #-}

module Hist where

import Prelude hiding (foldr)

import Data.Foldable
import Data.Traversable
import Control.Applicative

data Hist b v = HLeaf v | HNode (Hist b v) b (Hist b v)
    deriving (Show, Eq, Ord)

instance Functor (Hist b) where
    fmap f (HLeaf x) = HLeaf $ f x
    fmap f (HNode h b h') = HNode (fmap f h) b (fmap f h')

instance Foldable (Hist b) where
    foldr f x (HLeaf v) = f v x
    foldr f x (HNode h _ h') = foldr f (foldr f x h) h'

instance Traversable (Hist b) where
    traverse f (HLeaf x) = fmap HLeaf (f x)
    traverse f (HNode h b h') = flip HNode b <$> traverse f h <*> traverse f h'


fromList :: [(b,v)] -> Hist b v
fromList 0 (x:xs) = HLeaf x
fromList n xs 

binEdges :: Hist b v -> [b]
binEdges (HNode (HLeaf

toList :: b => Hist b v -> [(b,v)]
toList = foldr (:) []

{-
instance Applicative (Hist b) where
    pure v = HLeaf v
    h <*> h' = 



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


type Histogram b v = [HistBin b v]

fromListWithDefault :: Ord b => v -> [b] -> Histogram b v
fromListWithDefault _ [] = []
fromListWithDefault _ [_] = []
fromListWithDefault def (x:x':xs) = HistBin (Interval x x') def : fromListWithDefault def (x':xs)

fillBIfContains :: (Bin b, Ord a, Num v) => [b a v] -> a -> v -> [b a v]
fillBIfContains (b:bs) x wgt = if b `containsB` x
                            then b `fillB` wgt : bs
                            else b : fillBIfContains bs x wgt
fillBIfContains [] _ _ = []


instance Hist Histogram where
    fillH (bs) x wgt = fillBIfContains bs x wgt

    extractH (b:bs) x = if b `containsB` x
                                        then value b
                                        else extractH bs x

    extractH [] _ = error "no bin containing value found in histogram."


th1d :: [Double] -> Histogram Double (U Double)
th1d xs = fromListWithDefault (U 0.0 0.0) (minBound:xs ++ maxBound)
-}
