{-|
Module      : Data.Algorithm.PP.Perm.Property.Parity
Description : Parity properties
Copyright   : (c) Stéphane Vialette, 2021-2022
License     : GPL-3
Maintainer  : stephane.vialette@univ-eiffel.fr
Stability   : experimental

-}

module Data.Algorithm.PP.Perm.Property.Parity (
--   -- * First and last
--   isStartingWithEven
-- , isStartingWithOdd
-- , isTerminatingWithEven
-- , isTerminatingWithOdd

--   -- * Runs
-- , hasEvenRun
-- , hasOddRun
-- , isStartingWithEvenRun
-- , isStartingWithOddRun
-- , isTerminatingWithEvenRun
-- , isTerminatingWithOddRun

--   -- * Parity alternating
-- , isParityAlternating
-- , isParityAlternatingFirstOdd
-- , isParityAlternatingFirstEven
-- , isParityAlternatingLastOdd
-- , isParityAlternatingLastEven
-- , isParityAlternatingFirstEvenLastEven
-- , isParityAlternatingFirstEvenLastOdd
-- , isParityAlternatingFirstOddLastEven
-- , isParityAlternatingFirstOddLastOdd
) where

import qualified Data.List     as L
import qualified Data.Foldable as F

import qualified Data.Algorithm.PP.Perm       as PP.Perm
-- import qualified Data.Algorithm.PP.Utils.List as PP.Utils.List

-- {- | 'hasEvenRun' @p@ returns true iff the even elements of the permutation @p@ occur consecutively.

-- >>> let p = PP.Perm.mk [1,3,2,4,5] in hasEvenRun p
-- True
-- >>> let p = PP.Perm.mk [1,3,2,4,5,6] in hasEvenRun p
-- False
-- -}
-- hasEvenRun :: PP.Perm.Perm -> Bool 
-- hasEvenRun = F.all odd . L.dropWhile even . L.dropWhile odd . PP.Perm.getList

-- {- | 'hasOddRun' @p@ returns true iff the odd elements of permutation @p@ occur consecutively.

-- >>> let p = PP.Perm.mk [2,4,1,3,5,6] in hasOddRun p
-- True
-- >>> let p = PP.Perm.mk [2,4,1,3,6,5] in hasOddRun p
-- False
-- -}
-- hasOddRun :: PP.Perm.Perm -> Bool 
-- hasOddRun = F.all even . L.dropWhile odd . L.dropWhile even . PP.Perm.getList

-- {- |'isStartingWithEvenRun' @p@ return true is the permutation @p@ starts with a run containing all 
-- its even entries.

-- >>> import qualified Data.Algorithm.PP.Perm as PP.Perm
-- >>> let p = PP.Perm.mk [2,3,4,6,1,5] in isStartingWithEvenRun p
-- False
-- >>> let p = PP.Perm.mk [2,4,6,3,1,5] in isStartingWithEvenRun p
-- True
-- -}
-- isStartingWithEvenRun :: PP.Perm.Perm -> Bool 
-- isStartingWithEvenRun = F.all odd . L.dropWhile even . PP.Perm.getList

-- {- |'isStartingWithOddRun' @p@ return true is the permutation @p@ starts with a run containing all 
-- its odd entries.

-- >>> import qualified Data.Algorithm.PP.Perm as PP.Perm
-- >>> let p = PP.Perm.mk [5,1,2,3,4,6] in isStartingWithOddRun p
-- False
-- >>> let p = PP.Perm.mk [5,1,3,2,4,6] in isStartingWithOddRun p
-- True
-- -}
-- isStartingWithOddRun :: PP.Perm.Perm -> Bool 
-- isStartingWithOddRun = F.all even . L.dropWhile odd . PP.Perm.getList

-- isTerminatingWithEvenRun :: PP.Perm.Perm -> Bool 
-- isTerminatingWithEvenRun = F.all odd . L.dropWhile even . L.reverse . PP.Perm.getList

-- isTerminatingWithOddRun :: PP.Perm.Perm -> Bool 
-- isTerminatingWithOddRun = F.all even . L.dropWhile odd . L.reverse . PP.Perm.getList


-- {- | 'isParityAlternating' @p@ returns true iff permutation @p@ is parity alternating.

-- >>> isParityAlternating $ Perm.mk [1,2,3,4,5,6]
-- True
-- >>> isParityAlternating $ Perm.mk [2,1,4,3,6,5]
-- True
-- >>> isParityAlternating $ Perm.mk [2,1,3,6,5,4]
-- False
-- -}
-- isParityAlternating :: PP.Perm.Perm -> Bool
-- isParityAlternating p = F.and $ L.zipWith (/=) oes (L.tail oes)
--   where
--     oes = L.map (`mod` 2) $ PP.Perm.getList p

-- -- Auxiliary functions
-- firstParity :: (Int -> Bool) -> PP.Perm.Perm -> Bool
-- firstParity f =  maybe False f . PP.Perm.first

-- lastParity :: (Int -> Bool) -> PP.Perm.Perm -> Bool
-- lastParity f =  maybe False f . PP.Perm.last

-- {- | 'isStartingWithEven' @p@ returns true iff permutation @p@ starts with an even.

-- -}
-- isStartingWithEven :: PP.Perm.Perm -> Bool
-- isStartingWithEven = firstParity even

-- {- | 'isStartingWithOdd' @p@ returns true iff permutation @p@ starts with an odd.

-- -}
-- isStartingWithOdd :: PP.Perm.Perm -> Bool
-- isStartingWithOdd = firstParity odd 

-- {- | 'isTerminatingWithEven' @p@ returns true iff permutation @p@ terminates with an even.

-- -}
-- isTerminatingWithEven :: PP.Perm.Perm -> Bool
-- isTerminatingWithEven = lastParity even

-- {- | 'isTerminatingWithOdd' @p@ returns true iff permutation @p@ terminates with an odd.

-- -}
-- isTerminatingWithOdd :: PP.Perm.Perm -> Bool
-- isTerminatingWithOdd = lastParity odd

-- {- | 'isParityAlternatingFirstOdd' @p@ returns true iff permutation @p@ is parity alternating
-- and starts with an odd integer.

-- -}
-- isParityAlternatingFirstOdd :: PP.Perm.Perm -> Bool
-- isParityAlternatingFirstOdd p = isStartingWithOdd p && isParityAlternating p

-- {- | 'isParityAlternatingFirstEven' @p@ returns true iff permutation is parity alternating
-- and starts with an even integer.

-- -}
-- isParityAlternatingFirstEven :: PP.Perm.Perm -> Bool
-- isParityAlternatingFirstEven p = isStartingWithEven p && isParityAlternating p

-- {- | 'isParityAlternatingLastOdd' @p@ returns true iff permutation is parity alternating
-- and ends with an odd integer.

-- -}
-- isParityAlternatingLastOdd :: PP.Perm.Perm -> Bool
-- isParityAlternatingLastOdd p = isTerminatingWithOdd p && isParityAlternating p

-- {- | 'isParityAlternatingLastEven' @p@ returns true iff permutation is parity alternating
-- and ends with an even integer.

-- -}
-- isParityAlternatingLastEven :: PP.Perm.Perm -> Bool
-- isParityAlternatingLastEven p = isTerminatingWithEven p && isParityAlternating p

-- {- | 'isParityAlternatingFirstEvenLastEven' @p@ returns true iff permutation is parity
-- alternating, starts with an even integer and ends with an even integer.
-- -}
-- isParityAlternatingFirstEvenLastEven :: PP.Perm.Perm -> Bool
-- isParityAlternatingFirstEvenLastEven p = isStartingWithEven p && isTerminatingWithEven p && isParityAlternating p

-- {- | 'isParityAlternatingFirstEvenLastEven' @p@ returns true iff permutation is parity
-- alternating, starts with an even integer and ends with an odd integer.
-- -}
-- isParityAlternatingFirstEvenLastOdd :: PP.Perm.Perm -> Bool
-- isParityAlternatingFirstEvenLastOdd p = isStartingWithEven p && isTerminatingWithOdd p && isParityAlternating p

-- {- | 'isParityAlternatingFirstEvenLastEven' @p@ returns true iff permutation is parity
-- alternating, starts with an odd integer and ends with an even integer.
-- -}
-- isParityAlternatingFirstOddLastEven :: PP.Perm.Perm -> Bool
-- isParityAlternatingFirstOddLastEven p = isStartingWithOdd p && isTerminatingWithEven p && isParityAlternating p

-- {- | 'isParityAlternatingFirstEvenLastEven' @p@ returns true iff permutation is parity
-- alternating, starts with an odd integer and ends with an odd integer.
-- -}
-- isParityAlternatingFirstOddLastOdd :: PP.Perm.Perm -> Bool
-- isParityAlternatingFirstOddLastOdd p = isStartingWithOdd p && isTerminatingWithOdd p && isParityAlternating p
