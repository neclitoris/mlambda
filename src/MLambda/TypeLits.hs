{-# LANGUAGE RequiredTypeArguments #-}

-- |
-- Module      : MLambda.TypeLits
-- Description : Interaction with the type-level.
-- Copyright   : (c) neclitoris, TurtlePU, 2025
-- License     : BSD-3-Clause
-- Maintainer  : nas140301@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module reexports typeclasses from 'GHC.TypeLits'
-- and defines more convenient interface
-- using new @RequiredTypeArguments@ language extension.
module MLambda.TypeLits (Natural, KnownNat, natVal, enumSize) where

import GHC.TypeLits (KnownNat, Natural, fromSNat, natSing)

-- | Get a term-level representation of a type-level 'GHC.TypeLits.Nat'.
natVal :: forall n -> (KnownNat n, Num a) => a
natVal n = fromInteger $ fromSNat $ natSing @n

enumSize :: forall a -> (Bounded a, Enum a) => Int
enumSize t = fromEnum (maxBound @t) - fromEnum (minBound @t) + 1
