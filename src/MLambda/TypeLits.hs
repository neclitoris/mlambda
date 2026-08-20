{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE StandaloneKindSignatures #-}

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
module MLambda.TypeLits
  ( module GHC.TypeNats
  , natVal
  , enumSize
  , Unify
  , PNat (..)
  , PLength
  , Peano
  , RNat (..)
  , RPNat (..)
  , Fin (..)
  , Fins
  , At(..)
  , type (!)
  , ReifiedNat
  , rnat
  , rpnat
  ) where

import Data.Kind
import Data.Proxy (Proxy (Proxy))
import GHC.TypeError (ErrorMessage (..), TypeError)
import GHC.TypeNats hiding (natVal)

-- | Get a term-level representation of a type-level 'GHC.TypeLits.Nat'.
natVal :: forall n -> (KnownNat n, Num a) => a
natVal n = fromInteger $ toInteger $ fromSNat $ natSing @n

-- | Get a number of elements in a bounded enumeration type.
enumSize :: forall a -> (Bounded a, Enum a) => Int
enumSize t = fromEnum (maxBound @t) - fromEnum (minBound @t) + 1

-- | @Unify n a b@ unifies @a@ and @b@ which are then named @n@ on error.
type family Unify n a b where
  Unify _ a a = a
  Unify n a b = TypeError (Text n :<>: Text " are not equal:" :$$: ShowType a :$$: ShowType b)

-- | Peano naturals.
data PNat = PZ | PS PNat

-- | Compute length of a type-level list as a Peano natural.
type family PLength xs where
  PLength '[] = PZ
  PLength (_:xs) = PS (PLength xs)

-- | Compute Peano representation from type-level natural.
type family Peano n where
  Peano 0 = PZ
  Peano n = PS (Peano (n - 1))

-- | A reflection of a type-level natural into term-level.
data RNat n where
  RZ :: RNat 0
  RS :: RNat n -> RNat (n + 1)

-- | A reflection of a type-level Peano natural into term-level.
data RPNat n where
  RPZ :: RPNat PZ
  RPS :: RPNat n -> RPNat (PS n)

-- | Finite list index.
type Fin :: [k] -> Type
data Fin (l :: [k :: Type]) where
  FZ :: Fin (x : xs)
  FS :: Fin xs -> Fin (x : xs)

type (!) :: forall l -> Fin l -> Type
type family (!) (l :: [k :: Type]) (i :: Fin l) where
  (x ': _) ! FZ = x
  (_ ': xs) ! (FS i) = xs ! i

type At :: forall {k} {l :: [k]} . Fin l -> Type
data At (i :: Fin l) where
  At :: l ! i -> At i

type Map :: forall k l . (k -> l) -> [k] -> [l]
type family Map f l where
  Map _ '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

-- | Creates a list of indices into a type-level list.
type Fins :: forall {k} (l :: [k]) . [k] -> [Fin l]
type family Fins (l :: [k :: Type]) where
  Fins '[] = '[]
  Fins (x ': xs) = FZ ': Map FS (Fins xs)

-- | A stronger variant of 'KnownNat' which enables induction on type-level naturals.
class ReifiedNat n where
  rnat0 :: RNat n
  rpnat0 :: Proxy n -> RPNat (Peano n)

-- | A function to link type-level natural with its term-level reification.
rnat :: forall n -> ReifiedNat n => RNat n
rnat n = rnat0 @n

-- | A function to link type-level natural
-- with reification of its Peano encoding.
rpnat :: forall n -> ReifiedNat n => RPNat (Peano n)
rpnat n = rpnat0 @n Proxy

instance ReifiedNat 0 where
  rnat0 = RZ
  rpnat0 _ = RPZ

instance (ReifiedNat m, n ~ m + 1, Peano n ~ PS (Peano m)) => ReifiedNat n where
  rnat0 = RS rnat0
  rpnat0 _ = RPS (rpnat0 @m Proxy)
