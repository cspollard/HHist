-- {-# LANGUAGE FlexibleInstances #-}

module Data.Hist where

-- import Control.Arrow (first)
-- import qualified Data.Map as M
-- import Bin
-- import Uncertain
import Prelude hiding (foldr)
import qualified Prelude as P
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import qualified Control.Applicative as A
import qualified Control.Monad as M
import qualified Data.Default as D

-- basically a BST, but no insertion
data Hist b v =
      HBin (Hist b v) b (Hist b v)
    | HTip v
    deriving (Show, Eq, Ord)

singleton :: v -> Hist b v
singleton = HTip

insertAsc :: Ord b => v ->  b -> Hist b v -> Hist b v
insertAsc v b (HBin l p r) =
    if b < p
        then HBin (insertAsc v b l) p r
        else HBin l p (insertAsc v b r)

-- this always assumes the new data goes to the right of the old data.
insertAsc v b l = HBin (HTip v) b l

-- TODO
fromAscList :: Ord b => [b] -> [v] -> Hist b v
fromAscList bs vs = P.foldr (uncurry insertAsc) (singleton $ head vs) $ zip (tail vs) bs

toList :: Hist b v -> ([b], [v])
toList h = (foldrBins (:) [] h, F.foldr (:) [] h)

instance Functor (Hist b) where
    fmap f (HBin l x r) = HBin (fmap f l) x (fmap f r)
    fmap f (HTip v) = HTip (f v)

mapBins :: (b -> c) -> Hist b v -> Hist c v
mapBins f (HBin l x r) = HBin (mapBins f l) (f x) (mapBins f r)
mapBins _ (HTip v) = HTip v

instance Monad (Hist b) where
    return = HTip
    (HBin l x r) >>= f = HBin (l >>= f) x (r >>= f)
    (HTip v) >>= f = f v

instance A.Applicative (Hist b) where
    pure = HTip
    (<*>) = M.ap

instance F.Foldable (Hist b) where
    foldr f y (HBin l _ r) = F.foldr f (F.foldr f y l) r
    foldr f y (HTip v) = f v y

foldrBins :: (b -> c -> c) -> c -> Hist b v -> c
foldrBins f y (HBin l x r) = foldrBins f (f x (foldrBins f y l)) r
foldrBins _ y _ = y

instance T.Traversable (Hist b) where
    traverse f (HBin l x r) = flip HBin x A.<$> T.traverse f l A.<*> T.traverse f r
    traverse f (HTip v) = HTip A.<$> f v

instance D.Default v => D.Default (Hist b v) where
    def = singleton D.def

{-
class Hist h where
    fill :: (Ord b, Num v) => h b v -> b -> v -> h b v

instance Hist M.Map where
    fill h x w = M.adjust (+ w) x h

type Hist b v = M.Map (Bin b) v
type Hist2D b v = M.Map (Bin b, Bin b) v

histWithDefault :: Ord b => [Bin b] -> v -> Hist b v
histWithDefault bins def = M.fromList $ zip bins (repeat def)

fillWeight :: (Ord b, Num v) => Hist b v -> b -> v -> Hist b v
fillWeight h x w = M.adjust (w +) (Val x) h

-- fill :: (Ord b, Num v) => Hist b v -> b -> Hist b v
-- fill h x = fillWeight h x 1

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
-}
