module Data.Algorithm.PP.Perm (
  -- * Type
  Perm

  -- * Making
, mk
, mkId
) where

import qualified Data.Algorithm.PP.Perm.Inner as PP.Perm.Inner

type Perm = PP.Perm.Inner.PermInner Int

mk :: (Foldable t, Ord a) => t a -> Perm
mk = PP.Perm.Inner.mk

mkId :: Int -> Perm 
mkId = PP.Perm.Inner.mkId
