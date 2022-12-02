module Data.Algorithm.PP.Perm.Generator.Basic (

  perms
, steinhausJohnsonTrotterPerms
, myrvoldRuskey1Perms
, myrvoldRuskey2Perms
) where

import qualified Data.Foldable as F
import qualified Data.List     as L
import           Data.Maybe
-- import qualified Data.Vector   as V
-- import qualified Data.Tuple    as T

import qualified Data.Algorithm.PP.Perm      as PP.Perm
import qualified Data.Algorithm.PP.Perm.Rank as PP.Perm.Rank

{- |'perms' @n@

>>> mapM_ print (perms 4)
[1,2,3,4]
[2,1,3,4]
[3,2,1,4]
[2,3,1,4]
[3,1,2,4]
[1,3,2,4]
[4,3,2,1]
[3,4,2,1]
[3,2,4,1]
[4,2,3,1]
[2,4,3,1]
[2,3,4,1]
[4,1,2,3]
[1,4,2,3]
[1,2,4,3]
[4,2,1,3]
[2,4,1,3]
[2,1,4,3]
[4,1,3,2]
[1,4,3,2]
[1,3,4,2]
[4,3,1,2]
[3,4,1,2]
[3,1,4,2]

-}
perms :: Int -> [PP.Perm.Perm]
perms n = fmap PP.Perm.mk $ L.permutations  [1..n]

{- |'steinhausJohnsonTrotterPerms' @n@

>>> mapM_ print (steinhausJohnsonTrotterPerms 4)
[1,2,3,4]
[2,1,3,4]
[2,3,1,4]
[2,3,4,1]
[1,3,2,4]
[3,1,2,4]
[3,2,1,4]
[3,2,4,1]
[1,4,3,2]
[4,1,3,2]
[4,3,1,2]
[4,3,2,1]
[1,3,4,2]
[3,1,4,2]
[3,4,1,2]
[3,4,2,1]
[1,4,2,3]
[4,1,2,3]
[4,2,1,3]
[4,2,3,1]
[1,2,4,3]
[2,1,4,3]
[2,4,1,3]
[2,4,3,1]
-}
steinhausJohnsonTrotterPerms :: Int -> [PP.Perm.Perm]
steinhausJohnsonTrotterPerms n = fmap PP.Perm.mk $ go [1..n]
  where
    go []       = [[]]
    go (x : xs) = L.concat (L.map (spread x) (L.permutations xs))

    spread x []         = [[x]]
    spread x (x' : xs) = (x : x' : xs) : (L.map (x' :) (spread x xs))


{- |'myrvoldRuskey1Perms'

>>> mapM_ print (myrvoldRuskey1Perms 4)
[2,3,4,1]
[4,3,1,2]
[2,4,1,3]
[2,3,1,4]
[3,4,2,1]
[3,1,4,2]
[4,1,2,3]
[3,1,2,4]
[2,4,3,1]
[4,1,3,2]
[2,1,4,3]
[2,1,3,4]
[3,2,4,1]
[3,4,1,2]
[4,2,1,3]
[3,2,1,4]
[4,3,2,1]
[1,3,4,2]
[1,4,2,3]
[1,3,2,4]
[4,2,3,1]
[1,4,3,2]
[1,2,4,3]
[1,2,3,4]
-}
myrvoldRuskey1Perms :: Int -> [PP.Perm.Perm]
myrvoldRuskey1Perms n = fmap (fromJust .PP.Perm.Rank.unrank1 n) [0..product [1..n]-1]

{- |'myrvoldRuskey2Perms'

>>> mapM_ print (myrvoldRuskey2Perms 4)
[2,3,4,1]
[3,2,4,1]
[3,4,2,1]
[4,3,2,1]
[2,4,3,1]
[4,2,3,1]
[4,3,1,2]
[3,4,1,2]
[3,1,4,2]
[1,3,4,2]
[4,1,3,2]
[1,4,3,2]
[2,4,1,3]
[4,2,1,3]
[4,1,2,3]
[1,4,2,3]
[2,1,4,3]
[1,2,4,3]
[2,3,1,4]
[3,2,1,4]
[3,1,2,4]
[1,3,2,4]
[2,1,3,4]
[1,2,3,4]
-}
myrvoldRuskey2Perms :: Int -> [PP.Perm.Perm]
myrvoldRuskey2Perms n = fmap (fromJust .PP.Perm.Rank.unrank2 n) [0..product [1..n]-1]