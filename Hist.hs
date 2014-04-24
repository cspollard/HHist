module Hist where

import Data.Monoid ((<>))
import qualified Data.Map as M

data Bin a = Bin a a | Val a deriving (Eq, Show)

instance (Ord a) => Ord (Bin a) where
    compare (Bin w x) (Bin y z) = compare w y <> compare x z

    compare (Bin x y) (Val z) = case (compare z x, compare z y) of
                                    (GT, LT) -> EQ
                                    (EQ, LT) -> EQ
                                    (LT, _) -> LT
                                    _ -> GT

    compare v@(Val _) b@(Bin _ _) = compare b v

    compare (Val x) (Val y) = compare x y


type Hist b v = M.Map (Bin b) v

binsFromList :: [a] -> [Bin a]
binsFromList [] = []
binsFromList [_] = []
binsFromList (x:y:xs) = Bin x y : binsFromList (y:xs)

histWithDefault :: Ord b => [b] -> v -> Hist b v
histWithDefault bins def = M.fromList $ zip (binsFromList bins) (repeat def)

fillWeight :: (Ord b, Num v) => Hist b v -> b -> v -> Hist b v
fillWeight h x w = M.adjust (w +) (Val x) h
