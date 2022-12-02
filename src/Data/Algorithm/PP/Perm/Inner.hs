module Data.Algorithm.PP.Perm.Inner (
  -- * Type
  PermInner

  -- * Making
, mk
, mkId
) where

import           Control.Arrow (&&&)
import qualified Data.Foldable as F
import           Data.Function (on)
import qualified Data.List     as L
import qualified Data.Vector   as V
import qualified Data.Tuple    as T

newtype PermInner a = PermInner { elements :: V.Vector a }

instance (Show a) => Show (PermInner a) where
  show = show . V.toList . elements

dup x = (x, x)

nextPerm = F.foldr f Nothing . L.zip . second L.tail . dup . L.zip [1..] . V.toList . elements
  where
    f ((i, xi), (j, xj)) Nothing = if xi < xj then Just i else Nothing

mk :: (Foldable t, Ord a, Num b, Enum b) => t a -> PermInner b
mk = PermInner . V.fromList . reduce . F.toList

mkId :: (Num a) => Int -> PermInner a 
mkId = PermInner . V.enumFromN 1

reduce :: (Ord a, Num b, Enum b) => [a] -> [b]
reduce = L.map T.fst . L.sortBy g . L.zip [1..] . L.sortBy f . L.zip [1..]
  where
    f = compare `on` T.snd
    g = compare `on` (T.fst . T.snd)
