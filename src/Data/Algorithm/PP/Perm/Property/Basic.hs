{-|
Module      : Data.Algorithm.PP.Perm.Property.Basic
Description : Basic properties
Copyright   : (c) Stéphane Vialette, 2021-2023
License     : GPL-3
Maintainer  : stephane.vialette@univ-eiffel.fr
Stability   : experimental

-}

module Data.Algorithm.PP.Perm.Property.Basic (
  -- * Monotone
--   isIncreasing
-- , isSorted
-- , isDecreasing
-- , isReverseSorted
-- , isMonotone

isDerangement
-- , isInvolution
-- , isFixedPointFreeInvolution
) where

import qualified Data.List     as L
import qualified Data.Foldable as F

-- import qualified Data.Algorithm.PP.Geometry.Point as PP.Geometry.Point
import qualified Data.Algorithm.PP.Perm           as PP.Perm
-- import qualified Data.Algorithm.PP.Perm.Cycle     as PP.Perm.Cycle
-- import qualified Data.Algorithm.PP.Utils.List.Basic as PP.Utils.List
--     ( isIncreasing, isDecreasing )

-- {- | 'isIncreasing' @p@ returns @True@ if the permutation @p@ is increasing. -}
-- isIncreasing :: PP.Perm.Perm -> Bool 
-- isIncreasing = PP.Utils.List.isIncreasing . PP.Perm.getList

-- {- | Alias for 'isIncreasing' -}
-- isSorted :: PP.Perm.Perm -> Bool
-- isSorted = isIncreasing

-- {- | 'isDecreasing' @p@ returns @True@ if the permutation @p@ is isDecreasing. -}
-- isDecreasing :: PP.Perm.Perm -> Bool 
-- isDecreasing = PP.Utils.List.isDecreasing . PP.Perm.getList

-- {- | Alias for 'isDecreasing' -}
-- isReverseSorted :: PP.Perm.Perm -> Bool
-- isReverseSorted = isDecreasing

-- {- | 'isMonotone' @p@ returns @True@ if the permutation @p@ is is monotone
-- (i.e. @p@ is either increasing or decreasing).
-- -}
-- isMonotone :: PP.Perm.Perm -> Bool
-- isMonotone p = isIncreasing p || isDecreasing p

{- | 'isDerangement' @p@ returns @True@ if the permutation @p@ is a derangement
(i.e. @p@ is a permutation that has no fixed points).
-}
isDerangement :: PP.Perm.Perm -> Bool
isDerangement = F.all (uncurry (/=)) . PP.Perm.toIdxList

-- {- | 'isInvolution' @p@ returns @True@ if the permutation @p@ is an involution
-- (i.e. @p@ is a permutation which does not contain any permutation cycles of length >2).
-- -}
-- isInvolution :: PP.Perm.Perm -> Bool
-- isInvolution = F.all (<= 2) . fmap L.length . PP.Perm.Cycle.cycles


-- {- | 'isFixedPointFreeInvolution' @p@ returns @True@ if the permutation @p@ is a
-- fiexed point free involution
-- (i.e. @p@ is a permutation which does not contain any permutation cycles of length /= 2).
-- -}
-- isFixedPointFreeInvolution :: PP.Perm.Perm -> Bool
-- isFixedPointFreeInvolution = F.all (== 2) . fmap L.length . PP.Perm.Cycle.cycles



