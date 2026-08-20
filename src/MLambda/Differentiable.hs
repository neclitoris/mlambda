-- |
-- Module      : MLambda.Differentiable
-- Description : Implements differentiation machinery.
-- Copyright   : (c) neclitoris, 2026
-- License     : BSD-3-Clause
-- Maintainer  : nas140301@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module contains definition of `Differentiable` type class.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
module MLambda.Differentiable
  ( Functional(..)
  , Differentiable(..)
  , Matmul(..)
  ) where

import MLambda.Index
import MLambda.Matrix
import MLambda.NDArr
import MLambda.TypeLits

import Data.Kind
import Data.List.Singletons
import Data.Vinyl
import Numeric.Netlib.Class

import Prelude hiding (Floating)


type family ArgsL (i :: [[Natural]]) e :: [Type] where
  ArgsL '[] e = '[]
  ArgsL (x ': xs) e = NDArr x e : ArgsL xs e

type family Args (i :: [[Natural]]) e :: Type where
  Args i e = Rec At (Fins @(ArgsL i e) (ArgsL i e))

type family Fun (i :: [[Natural]]) (o :: [Natural]) e :: Type where
  Fun i o e = Args i e -> NDArr o e

class Functional f i o e where
  ($$) :: f -> Fun i o e

class Functional f i o e => Differentiable f i o e where
  d :: f -> Args i e -> Index o -> Args i e

data Matmul = Matmul

instance (KnownNat m, KnownNat n, KnownNat k, Floating e) => Functional Matmul '[[m,n], [n,k]] '[m,k] e where
  _ $$ (At a :& At b :& RNil) = a `cross` b

instance (1 <= m, 1 <= n, 1 <= k, KnownNat m, KnownNat n, KnownNat k, Floating e) => Differentiable Matmul '[[m,n], [n,k]] '[m,k] e where
  d _ (At a :& At b :& RNil) (i :. j) = At a' :& At b' :& RNil
    where
      a' = fromIndex \(k :. l) -> if k == i then b `at` (l :. j) else 0
      b' = fromIndex \(k :. l) -> if l == j then a `at` (i :. k) else 0

data (:.:) f1 f2 = f1 :.: f2

instance (Functional f1 i1 o1 e, Functional f2 (o1 : i2) o2 e, i ~ i1 ++ i2) => Functional (f1 :.: f2) i o2 e where
  (f1 :.: f2) $$ r = undefined

