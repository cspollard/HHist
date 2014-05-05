{-# LANGUAGE TypeOperators #-}

module Hist where

import Control.Arrow
import Control.Applicative
import qualified Data.Map as M
import Data.Monoid
import Data.Maybe (fromJust)


data Bin r = Range r r | Val r deriving (Eq, Show, Read)

type ValUncert v = (v, v)

instance Functor Bin where
    fmap f (Range x y) = Range (f x) (f y)
    fmap f (Val v) = Val (f v)


-- most important part: for filling histograms
instance (Ord a) => Ord (Bin a) where
    compare (Range w x) (Range y z) = let c = compare w y in
                                        case c of
                                            EQ -> compare x z
                                            _ -> c

    compare (Range x y) (Val z) = case (compare z x, compare z y) of
                                    (GT, LT) -> EQ
                                    (EQ, LT) -> EQ
                                    (LT, _) -> LT
                                    _ -> GT

    compare v@(Val _) r@(Range _ _) = compare r v

    compare (Val x) (Val y) = compare x y


{-
instance Applicative Bin where
    pure = Val

    Range f g <*> Range x y = Range (f x) (g y)
    Val f <*> Val x = Val (f x)

    Range f g <*> Val x = Range (f x) (g x)
    Val f <*> Range x y = Range (f x) (f y)
-}


type Hist r v = M.Map (Bin r) v
type Hist1D v = M.Map (Bin Double) v
type Hist2D v = M.Map (Bin Double) v



binsFromList :: [a] -> [Bin a]
binsFromList [] = []
binsFromList [_] = []
binsFromList (x:y:xs) = Range x y : binsFromList (y:xs)

histWithDefaultContent :: Ord b => [b] -> v -> Hist b v
histWithDefaultContent bins def = M.fromList $ zip (binsFromList bins) (repeat def)

fillWeight :: (Ord b, Num v) => Hist b v -> b -> v -> Hist b v
fillWeight h x w = M.adjust (w +) (Val x) h


fill :: (Ord b, Num v) => Hist b v -> b -> Hist b v
fill h x = M.adjust (1 +) (Val x) h

midPoint :: (Fractional b) => Bin b -> b
midPoint (Range x y) = (x + y)/2.0
midPoint (Val x) = x

histToPoints :: (Fractional b) => Hist b v -> [(b, v)]
histToPoints h = map (first midPoint) $ M.toList h

mapBins :: Ord c => (b -> c) -> Hist b v -> Hist c v
mapBins f = M.mapKeys (fmap f)

mapContents :: (v -> w) -> Hist b v -> Hist b w
mapContents = M.map
