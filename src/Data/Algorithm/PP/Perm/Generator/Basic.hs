module Data.Algorithm.PP.Perm.Generator.Basic (
  -- * All permutations
  perms
, lexPerms
, insertPerms
, rotatePerms
, steinhausJohnsonTrotterPerms
, myrvoldRuskey1Perms
, myrvoldRuskey2Perms

  -- * Derangements
, derangements
, lexDerangements

  -- * Alternating
, altPerms
) where

import qualified Data.Foldable as F
import qualified Data.List     as L
import           Data.Maybe
-- import qualified Data.Vector   as V
-- import qualified Data.Tuple    as T

import qualified Data.Algorithm.PP.Perm          as PP.Perm
import qualified Data.Algorithm.PP.Perm.Property as PP.Perm.Property
import qualified Data.Algorithm.PP.Perm.Rank     as PP.Perm.Rank

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

{- |'lexPerms' @n@

>>> mapM_ print (lexPerms 4)
[1,2,3,4]
[1,2,4,3]
[1,3,2,4]
[1,3,4,2]
[1,4,2,3]
[1,4,3,2]
[2,1,3,4]
[2,1,4,3]
[2,3,1,4]
[2,3,4,1]
[2,4,1,3]
[2,4,3,1]
[3,1,2,4]
[3,1,4,2]
[3,2,1,4]
[3,2,4,1]
[3,4,1,2]
[3,4,2,1]
[4,1,2,3]
[4,1,3,2]
[4,2,1,3]
[4,2,3,1]
[4,3,1,2]
[4,3,2,1]
-}
lexPerms :: Int -> [PP.Perm.Perm]
lexPerms n = fmap PP.Perm.mk (go [1..n])
  where
    go [] = [[]]
    go xs = [i : j | i <- xs, j <- go (L.delete i xs)]

{- |'insertPerms' @n@

https://rosettacode.org/wiki/Permutations#Haskell

>>> mapM_ print (insertPerms 4)
[1,2,3,4]
[2,1,3,4]
[2,3,1,4]
[2,3,4,1]
[1,3,2,4]
[3,1,2,4]
[3,2,1,4]
[3,2,4,1]
[1,3,4,2]
[3,1,4,2]
[3,4,1,2]
[3,4,2,1]
[1,2,4,3]
[2,1,4,3]
[2,4,1,3]
[2,4,3,1]
[1,4,2,3]
[4,1,2,3]
[4,2,1,3]
[4,2,3,1]
[1,4,3,2]
[4,1,3,2]
[4,3,1,2]
[4,3,2,1]
-}
insertPerms :: Int -> [PP.Perm.Perm]
insertPerms n = fmap PP.Perm.mk (go [1..n])
  where
  go = F.foldr (F.concatMap . insertEverywhere) [[]]
    where 
      insertEverywhere x []         = [[x]]
      insertEverywhere x l@(y : ys) = (x:l) : fmap (y :) (insertEverywhere x ys)

{- |'rotatePerms' @n@

https://stackoverflow.com/questions/40097116/get-all-permutations-of-a-list-in-haskell

>>> mapM_ print (rotatePerms 4)
[1,2,3,4]
[2,3,4,1]
[3,4,1,2]
[4,1,2,3]
[1,3,4,2]
[3,4,2,1]
[4,2,1,3]
[2,1,3,4]
[1,4,2,3]
[4,2,3,1]
[2,3,1,4]
[3,1,4,2]
[1,2,4,3]
[2,4,3,1]
[4,3,1,2]
[3,1,2,4]
[1,4,3,2]
[4,3,2,1]
[3,2,1,4]
[2,1,4,3]
[1,3,2,4]
[3,2,4,1]
[2,4,1,3]
[4,1,3,2]
-}
rotatePerms :: Int -> [PP.Perm.Perm]
rotatePerms n = fmap PP.Perm.mk (go n [1..n])
  where
    go _ []       = [[]]
    go _ [x]      = [[x]]
    go n (x : xs) = go (n-1) xs >>= rotations n . (x :)

    rotate (x : xs) = xs ++ [x]

    rotations k xs = L.take k (L.iterate rotate xs)

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

{- |'derangements' @n@ returns all derangements of length @n@.

>>> mapM_ print (derangements 4)
[4,3,2,1]
[3,4,2,1]
[2,3,4,1]
[4,1,2,3]
[2,4,1,3]
[2,1,4,3]
[4,3,1,2]
[3,4,1,2]
[3,1,4,2]
-}
derangements :: Int -> [PP.Perm.Perm]
derangements = L.filter (PP.Perm.Property.isDerangement) . perms

{- |'lexDerangements' @n@ returns all derangements of length @n@ in 
  lexicographic order.

>>> mapM_ print (lexDerangements 4)
[2,1,4,3]
[2,3,4,1]
[2,4,1,3]
[3,1,4,2]
[3,4,1,2]
[3,4,2,1]
[4,1,2,3]
[4,3,1,2]
[4,3,2,1]
-}
lexDerangements :: Int -> [PP.Perm.Perm]
lexDerangements = L.filter (PP.Perm.Property.isDerangement) . lexPerms

{- |'altPerms' @n@ returns all alternating permutations of length @n@.

>>> mapM_ print (altPerms 4)
[1,3,2,4]
[1,4,2,3]
[2,3,1,4]
[2,4,1,3]
[3,4,1,2]
[2,1,4,3]
[3,1,4,2]
[3,2,4,1]
[4,1,3,2]
[4,2,3,1]
-}
altPerms :: Int -> [PP.Perm.Perm]
altPerms n
  | n == 1    = [PP.Perm.mk [1]]
  | otherwise = fmap PP.Perm.mk (altStartUp ++ altStartDown)
  where 
    altStartUp   = L.concat [altUp   i ([1..i-1] ++ [i+1..n]) | i <- [1..n-1]]
    altStartDown = L.concat [altDown i ([1..i-1] ++ [i+1..n]) | i <- [2..n]]
      where
        altUp :: Int -> [Int] -> [[Int]]
        altUp x [] = [[x]]
        altUp x xs = fmap (x :) xss
          where
            xss = L.concat [altDown x' (L.delete x' xs) | x' <- L.filter (> x) xs]

        altDown :: Int -> [Int] -> [[Int]]
        altDown x [] = [[x]]
        altDown x xs = fmap (x :) xss
          where
            xss = L.concat [altUp x' (L.delete x' xs) | x' <- L.filter (< x) xs]