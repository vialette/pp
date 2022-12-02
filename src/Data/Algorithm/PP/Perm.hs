module Data.Algorithm.PP.Perm (
  -- * Type
  Perm
, TPerm

  -- * Making
, mk
, mkId

  -- * Length information
, length
, null

  -- * 
, toList
, toIdxList
) where

import Prelude hiding (length, null)

import           Control.Arrow ( (&&&) )
import qualified Data.Foldable as F
import           Data.Function (on)
import qualified Data.List     as L
import qualified Data.Vector   as V
import qualified Data.Tuple    as T

type TPerm = Int

newtype Perm = Perm { toVector :: V.Vector TPerm }

instance Show Perm where
  show = show . toList

mk :: (Foldable t, Ord a) => t a -> Perm
mk = Perm . V.fromList . reduce . F.toList

mkId :: Int -> Perm
mkId = Perm . V.enumFromN 1

reduce :: (Ord a, Num b, Enum b) => [a] -> [b]
reduce = L.map T.fst . L.sortBy g . L.zip [1..] . L.sortBy f . L.zip [1..]
  where
    f = compare `on` T.snd
    g = compare `on` (T.fst . T.snd)

toList :: Perm -> [TPerm]
toList = V.toList . toVector

toIdxList :: Perm -> [(Int, TPerm)]
toIdxList = L.zip [1..] . toList

length :: Perm -> Int 
length = V.length . toVector

null :: Perm -> Bool
null = V.null . toVector
