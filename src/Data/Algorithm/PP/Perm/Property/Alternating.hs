{-|
Module      : Data.Algorithm.PP.Perm.Property.Alternating
Description : Basic properties of alternating permutations
Copyright   : (c) Stéphane Vialette, 2021-2023
License     : GPL-3
Maintainer  : stephane.vialette@univ-eiffel.fr
Stability   : experimental

Basic properties of permutations.
-}

module Data.Algorithm.PP.Perm.Property.Alternating (
--   isUpDownAlternating
-- , isDownUpAlternating
-- , isAlternating
-- , isBipartite
-- , isUpDownBipartite
-- , isDownUpBipartite
) where

import qualified Data.Foldable as F

-- import qualified Data.Algorithm.PP.Geometry.Point   as PP.Geometry.Point
-- import qualified Data.Algorithm.PP.Perm             as PP.Perm
-- import qualified Data.Algorithm.PP.Utils.List.Basic as PP.Utils.List
--     ( chunk3 )

-- -- helper function
-- isUpDownAlternating' :: [(Int, Int, Int)] -> Bool
-- isUpDownAlternating' []                 = True
-- isUpDownAlternating' ((i, j, k) : ijks) = i < j && j > k && isDownUpAlternating' ijks

-- -- helper function
-- isDownUpAlternating' :: [(Int, Int, Int)] -> Bool
-- isDownUpAlternating' []                 = True
-- isDownUpAlternating' ((i, j, k) : ijks) = i > j && j < k && isUpDownAlternating' ijks

-- {- | 'isUpDownAlternating' @p@ return true if the permutation @p@ is alternating
-- and starts with an up-step.

-- >>> import qualified Data.Algorithm.PP.Perm as PP.Perm
-- >>> let p = PP.Perm.mk [3,5,2,4,1] in isUpDownAlternating p
-- True
-- >>> let p = PP.Perm.mk [4,1,5,2,3] in isUpDownAlternating p
-- False
-- -}
-- isUpDownAlternating :: PP.Perm.Perm -> Bool
-- isUpDownAlternating = isUpDownAlternating' . PP.Utils.List.chunk3 . PP.Perm.getList

-- -- Auxilliary functions
-- isBipartite' :: (Int -> Bool) -> PP.Perm.Perm -> Bool
-- isBipartite' fparity pi =  F.all step $ PP.Perm.getPoints pi
--   where
--     b = PP.Perm.len pi `div` 2
--     step p
--       | fparity x = y <= b
--       | otherwise = y >  b
--       where
--         x = PP.Geometry.Point.getX p
--         y = PP.Geometry.Point.getY p

-- {- | 'isUpDownBipartite' \(\pi\) return true if permutation \(\pi\) is biparite and
--   \(\pi(1) < \pi(2)\).

-- -}
-- isUpDownBipartite :: PP.Perm.Perm -> Bool
-- isUpDownBipartite = isBipartite' odd

-- {- | 'isDownUpBipartite' \(\pi\) return true if permutation \(\pi\) is bipartite and
--   \(\pi(1) > \pi(2)\).

-- -}
-- isDownUpBipartite :: PP.Perm.Perm -> Bool
-- isDownUpBipartite = isBipartite' even

-- {- | 'isBipartite' \(\pi\) return true if the permutation \(\pi\) is bipartite.

-- -}
-- isBipartite :: PP.Perm.Perm -> Bool
-- isBipartite pi = isUpDownBipartite pi || isDownUpBipartite pi

-- {- | 'isDownUpAlternating' @p@ return @True@ if the permutation @p@ is alternating
-- and starts with a down-step.

-- >>> isDownUpAlternating $ mk [3,5,2,4,1]
-- False
-- >>> isDownUpAlternating $ mk [4,1,5,2,3]
-- True
-- -}
-- isDownUpAlternating :: PP.Perm.Perm -> Bool
-- isDownUpAlternating = isDownUpAlternating' . PP.Utils.List.chunk3 . PP.Perm.getList

-- {- | 'isAlternating' @p@ return @True@ if the permutation @p@ is alternating
-- (i.e. each entry of @p@ is alternately greater or less than the preceding entry).

-- >>> let p = PP.Perm.mk [3,5,2,4,1] in isAlternating p 
-- WAS True
-- NOW Variable not in scope: mk :: [a0] -> Perm
-- >>> let p = PP.Perm.mk [4,1,5,2,3] in isAlternating p
-- WAS True
-- NOW Variable not in scope: mk :: [a0] -> Perm
-- >>> let p = PP.Perm.mk [3,4,5,1,2] in isAlternating p
-- WAS False
-- NOW Variable not in scope: mk :: [a0] -> Perm
-- -}
-- isAlternating :: PP.Perm.Perm -> Bool
-- isAlternating p = isUpDownAlternating p || isDownUpAlternating p
