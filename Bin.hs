module Bin where

import Data.Monoid ((<>))

data Bin b = Range b b | Val b
    deriving (Eq, Show)

-- most important part: for filling histograms
instance (Ord a) => Ord (Bin a) where
    compare (Range w x) (Range y z) = compare w y <> compare x z


    compare (Range x y) (Val z) =
        case (compare z x, compare z y) of
            (GT, LT) -> EQ
            (EQ, LT) -> EQ
            (LT, _) -> LT
            _ -> GT


    compare v@(Val _) r@(Range _ _) =
        case compare r v of
            EQ -> EQ
            GT -> LT
            LT -> GT

    compare (Val x) (Val y) = compare x y


instance Functor Bin where
    fmap f (Range x y) = Range (f x) (f y)
    fmap f (Val v) = Val (f v)

midPoint :: (Fractional b) => Bin b -> b
midPoint (Range x y) = (x + y)/2.0
midPoint (Val _) = undefined

fromLowEdges :: [a] -> [Bin a]
fromLowEdges [] = []
fromLowEdges [_] = []
fromLowEdges (x:y:xs) = Range x y : fromLowEdges (y:xs)
