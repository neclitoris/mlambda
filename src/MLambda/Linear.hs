-- |
-- Module      : MLambda.Linear
-- Description : Linear maps.
-- Copyright   : (c) neclitoris, TurtlePU, 2025
-- License     : BSD-3-Clause
-- Maintainer  : nas140301@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module describes linear maps.
module MLambda.Linear (Additive(..), Module(..), LinearMap, LinearMap') where

import Data.Kind

-- | @Additive@ describes things you can add.
class Additive m where
  add :: m -> m -> m
  zero :: m

-- | @Module@ is a class describing modules over a ring @r@.
-- Since @`Num`@ describes commutative rings, this is both a right
-- and a left module.
class (Num r, Additive m) => Module r m where
  modMult :: r -> m -> m

instance {-# OVERLAPPABLE #-} Num r => Additive r where
  add = (+)
  zero = 0

instance Num r => Module r r where
  modMult = (*)

-- | This version of LinearMap allows one to pass an arbitrary additional
-- constraint on the type mapped over. Note that this can break things:
-- @LinearMap' ((~) r) s m r@ will allow you to do naughty things.
type LinearMap' (e :: Type -> Constraint) t s r =
  forall m . (Module r m, e m) => t m -> s m

class Empty t

instance Empty t

-- | @LinearMap@ forces linearity (in mathematical sense) in its argument
-- since you can't multiply an element of @m@ by itself, only by some @a@.
type LinearMap t s r = LinearMap' Empty t s r
