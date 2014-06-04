module Bin where

data Bin b v = Bin b v b

instance Functor (Bin b) where
    fmap f (Bin l x h ) = Bin l (f x) h

instance (Ord b) => Ord (Bin b v b) where

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

midPoint :: (Fractional b) => Bin b -> b
midPoint (Range x y) = (x + y)/2.0
midPoint (Val _) = undefined

fromLowEdges :: [a] -> [Bin a]
fromLowEdges [] = []
fromLowEdges [_] = []
fromLowEdges (x:y:xs) = Range x y : fromLowEdges (y:xs)
