module Data.Algorithm.PP.Perm.Generator.Basic (

  perms
, steinhausJohnsonTrotterPerms
) where

import qualified Data.List     as L
-- import qualified Data.Vector   as V
-- import qualified Data.Tuple    as T

import qualified Data.Algorithm.PP.Perm as PP.Perm

{- |'perms' @n@

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