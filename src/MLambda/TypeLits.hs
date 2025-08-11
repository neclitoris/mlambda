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
module MLambda.TypeLits (module GHC.TypeNats, natVal, enumSize, Unify) where

import GHC.TypeError (ErrorMessage (..), TypeError)
import GHC.TypeNats hiding (natVal)

-- | Get a term-level representation of a type-level 'GHC.TypeLits.Nat'.
natVal :: forall n -> (KnownNat n, Num a) => a
natVal n = fromInteger $ toInteger $ fromSNat $ natSing @n

-- | Get a number of elements in a bounded enumeration type.
enumSize :: forall a -> (Bounded a, Enum a) => Int
enumSize t = fromEnum (maxBound @t) - fromEnum (minBound @t) + 1

-- | Unify two type-level expressions. @name@ is used for error reporting.
type family Unify name (a :: k) (b :: k) where
  Unify _ a a = a
  Unify n a b = TypeError
    (Text n :<>: Text " are not equal:" :$$: ShowType a :$$: ShowType b)
