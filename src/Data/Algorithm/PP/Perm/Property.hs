{-|
Module      : Data.Algorithm.PP.Perm.Property
Description : Properties
Copyright   : (c) Stéphane Vialette, 2021-2023
License     : GPL-3
Maintainer  : stephane.vialette@univ-eiffel.fr
Stability   : experimental

Properties of permutations.
-}

module Data.Algorithm.PP.Perm.Property (
  -- Alternating
  module Data.Algorithm.PP.Perm.Property.Alternating

  -- * Basic properties
, module Data.Algorithm.PP.Perm.Property.Basic

  -- * Parity
, module Data.Algorithm.PP.Perm.Property.Parity
) where

import Data.Algorithm.PP.Perm.Property.Alternating
import Data.Algorithm.PP.Perm.Property.Basic
import Data.Algorithm.PP.Perm.Property.Parity
