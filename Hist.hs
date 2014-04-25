module Hist where

import Control.Arrow
import Control.Applicative
import qualified Data.Map as M

data Bin a = Range a a | Val a deriving (Eq, Show)



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


instance Functor Bin where
    fmap f (Range x y) = Range (f x) (f y)
    fmap f (Val v) = Val (f v)


instance Applicative Bin where
    pure = Val

    Range f g <*> Range x y = Range (f x) (g y)
    Val f <*> Val x = Val (f x)

    Range f g <*> Val x = Range (f x) (g x)
    Val f <*> Range x y = Range (f x) (f y)


type Hist b v = M.Map (Bin b) v

binsFromList :: [a] -> [Bin a]
binsFromList [] = []
binsFromList [_] = []
binsFromList (x:y:xs) = Range x y : binsFromList (y:xs)

histWithDefault :: Ord b => [b] -> v -> Hist b v
histWithDefault bins def = M.fromList $ zip (binsFromList bins) (repeat def)

fillWeight :: (Ord b, Num v) => Hist b v -> b -> v -> Hist b v
fillWeight h x w = M.adjust (w +) (Val x) h

fill :: (Ord b, Num v) => Hist b v -> b -> Hist b v
fill h x = fillWeight h x 1

midPoint :: (Fractional b) => Bin b -> b
midPoint (Range x y) = (x + y)/2.0
midPoint (Val x) = x

histToPoints :: (Fractional b) => Hist b v -> [(b, v)]
histToPoints h = map (first midPoint) $ M.toList h
