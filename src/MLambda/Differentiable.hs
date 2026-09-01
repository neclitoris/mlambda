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
{-# LANGUAGE RequiredTypeArguments #-}
module MLambda.Differentiable
  ( Functional(..)
  , Differentiable(..)
  , Matmul(..)
  ) where

import MLambda.Index
import MLambda.Matrix
import MLambda.NDArr
import MLambda.TypeLits

import Data.Bifunctor
import Data.Either.Singletons
import Data.Kind
import Data.List.Singletons
import Data.Singletons
import Data.Type.Equality
import Data.Vinyl hiding ((:~:))
import Numeric.Netlib.Class

import Unsafe.Coerce

import Prelude hiding (Floating)


type family ArgsL (i :: [[Natural]]) e :: [Type] where
  ArgsL '[] e = '[]
  ArgsL (x ': xs) e = NDArr x e : ArgsL xs e

type Args i e = Rec At (Fins (ArgsL i e))

type Fun i o e = Args i e -> NDArr o e

class Functional f i o e where
  ($$) :: f -> Fun i o e

class Functional f i o e => Differentiable f i o e where
  d :: f -> Args i e -> Args (Map (Apply (++@#@$) o) i) e

data Matmul = Matmul

instance (KnownNat m, KnownNat n, KnownNat k, Floating e) => Functional Matmul '[[m,n], [n,k]] '[m,k] e where
  _ $$ (At a :& At b :& RNil) = a `cross` b

instance (1 <= m, 1 <= n, 1 <= k, KnownNat m, KnownNat n, KnownNat k, Floating e) => Differentiable Matmul '[[m,n], [n,k]] '[m,k] e where
  d _ (At a :& At b :& RNil) = At a' :& At b' :& RNil
    where -- TODO: optimize both representation and performance of this
      a' = fromIndex \(i :. j :. k :. l) -> if k == i then b `at` (l :. j) else 0
      b' = fromIndex \(i :. j :. k :. l) -> if l == j then a `at` (i :. k) else 0

data (:.:) f g = f :.: g

splitRec :: forall {l :: [Type]} (l1 :: [Type]) (l2 :: [Type]) .
  (l ~ l1 ++ l2) => Sing l1 -> Sing l -> Rec At (Fins (l1 ++ l2)) -> (Rec At (Fins l1), Rec At (Fins l2))
splitRec SNil _ xs = (RNil, xs)
splitRec (SCons _ (sxs :: Sing xs)) (SCons _ ys) (At v :& r) =
  first (\lhs -> At v :& shiftFS (withSingI sxs singFins) lhs) $ splitRec @xs @l2 sxs ys (stripFS (withSingI ys singFins) r)

{-# NOINLINE[1] unFS #-}
{-# RULES "unFSnop" unFS = unsafeCoerce #-}
unFS :: At (FS i) -> At i
unFS (At x) = At x

{-# NOINLINE[1] doFS #-}
{-# RULES "doFSnop" doFS = unsafeCoerce #-}
doFS :: At i -> At (FS i)
doFS (At x) = At x

{-# NOINLINE[1] stripFS #-}
{-# RULES "stripFSnop" forall x . stripFS x = unsafeCoerce #-}
stripFS
  :: Sing is
  -> Rec At (Map (TyCon1 FS) is)
  -> Rec At is
stripFS SNil RNil               = RNil
stripFS (SCons _ sis) (x :& xs) = unFS x :& stripFS sis xs

{-# NOINLINE[1] shiftFS #-}
{-# RULES "shiftFSnop" forall x . shiftFS x = unsafeCoerce #-}
shiftFS
  :: Sing is
  -> Rec At is
  -> Rec At (Map (TyCon1 FS) is)
shiftFS SNil RNil               = RNil
shiftFS (SCons _ sis) (x :& xs) = doFS x :& shiftFS sis xs

instance
    ( Functional f1 i1 o1 e
    , Functional f2 (o1 : i2) o2 e
    , ArgsL i e ~ ArgsL i1 e ++ ArgsL i2 e, SingI (ArgsL i1 e), SingI (ArgsL i2 e))
  => Functional (f2 :.: f1) i o2 e where
  (f2 :.: f1) $$ r =
    let sl1 = sing @(ArgsL i1 e)
        sl2 = sing @(ArgsL i2 e)
        sl = sl1 %++ sl2
        (lhs, rhs) = splitRec @(ArgsL i1 e) @(ArgsL i2 e) sl1 sl r
     in ($$) @f2 @(o1 : i2) @o2 @e f2 (At (($$) @f1 @i1 @o1 @e f1 lhs) :& shiftFS (withSingI sl2 singFins) rhs)

