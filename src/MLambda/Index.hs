{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : MLambda.Index
-- Description : Type of multidimensional array indices.
-- Copyright   : (c) neclitoris, TurtlePU, 2025
-- License     : BSD-3-Clause
-- Maintainer  : nas140301@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module contains definition of 'Index' type of multidimensional array
-- indices along with its instances and public interface.
module MLambda.Index
  ( Index (E, (:.))
  , consIndex
  , concatIndex
  , IndexI (..)
  , Ix
  , pattern IxI
  , inst
  -- * Lifting of runtime dimesions into indices
  , singToIndexI
  , withIx
  ) where

import Data.Proxy (Proxy (..))

import MLambda.TypeLits

import Data.Bool.Singletons
import Data.List.Singletons
import Data.Singletons
import GHC.TypeLits.Singletons hiding (natVal)

-- | @Index dim@ is the type of indices of multidimensional arrays of dimensions @dim@.
-- Instances are provided for convenient use:
--
-- - number literals work as one-dimensional indices of any size
-- (if the number is outside the index range, modulo is taken, so -1 refers to the largest
-- index)
-- - @(:.)@ prepends a one-dimensional index to another index
-- - @`minBound`@ represents the smallest index (first element in an n-dimensional array),
-- @`maxBound`@ represents the largest (last element)
-- - @`Enum`@ provides means to iterate through indices in the same
-- order their respective elements are laid out in memory.
data Index (dim :: [Natural]) where
  E :: Index '[]
  I :: Int -> Index '[n]
  (:.) :: Index '[n] -> Index (d : ds) -> Index (n : d : ds)

deriving instance Eq (Index dim)
deriving instance Ord (Index dim)
deriving instance Show (Index dim)

infixr 5 :.

instance (KnownNat n, 1 <= n) => Num (Index '[n]) where
  fromInteger = I . fromInteger . (`mod` natVal n)
  abs = id
  signum _ = 1
  negate (I m) = I $ (`mod` natVal n) $ natVal n - m
  (I a) + (I b) = I $ (a + b) `mod` natVal n
  (I a) * (I b) = I $ (a * b) `mod` natVal n

instance Bounded (Index '[]) where
  minBound = E
  maxBound = E

instance (KnownNat n, 1 <= n, Bounded (Index d)) => Bounded (Index (n:d)) where
  minBound = 0 `consIndex` minBound
  maxBound = (-1) `consIndex` maxBound

instance Enum (Index '[]) where
  toEnum = const E
  fromEnum = const 0

instance (KnownNat n, 1 <= n, Enum (Index d), Bounded (Index d)) =>
  Enum (Index (n:d)) where
  toEnum ((`quotRem` enumSize (Index d)) -> (q, r)) = I q `consIndex` toEnum r
  fromEnum = \case
    I i -> i
    I q :. r -> q * enumSize (Index d) + fromEnum r
  succ = \case
    h@(I i) | h == maxBound -> error "Undefined succ"
            | otherwise -> I (succ i)
    h :. t | t == maxBound -> succ h :. minBound
           | otherwise -> h :. succ t
  pred = \case
    h@(I i) | h == minBound -> error "Undefined pred"
            | otherwise -> I (pred i)
    h :. t | t == minBound -> pred h :. maxBound
           | otherwise -> h :. pred t

-- | Prepend a single-dimensional index to multi-dimensional one
consIndex :: Index '[x] -> Index xs -> Index (x : xs)
consIndex (I x) = \case
  E -> I x
  I y -> I x :. I y
  I y :. xs -> I x :. I y :. xs

-- | Concatenate two indices together
concatIndex :: Index xs -> Index ys -> Index (xs ++ ys)
concatIndex = \case
  E -> id
  I x -> consIndex (I x)
  I x :. xs -> consIndex (I x) . concatIndex xs

-- | A helper type that holds instances for everything you can get by pattern matching on
-- an @`Index`@ value. If you need to get instances for a head/tail of an @`Index`@,
-- pattern matching on a value of this type will bring them into the scope.
--
-- > f :: Ix (d : ds) => Index (d : ds) -> r
-- > f (i :. j) = case inst @(d : ds) of
-- >                _ :.= _ -> f j  -- @f j@ can be called here because we have @Ix ds@ now
-- > f i        = ...
data IndexI (dim :: [Natural]) where
  EI :: IndexI '[]
  (:.=) ::
    (KnownNat n, 1 <= n, Ix ds) => Proxy n -> IndexI ds -> IndexI (n : ds)

data IxInstance (dim :: [Natural]) where
  IxInstance :: Ix dim => IxInstance dim

viewII :: IndexI dim -> IxInstance dim
viewII (II :.= _) = IxInstance
viewII EI         = IxInstance

{-# COMPLETE IxI #-}
pattern IxI :: () => Ix dim => IndexI dim
pattern IxI <- (viewII -> IxInstance) where
  IxI = inst

deriving instance Show (IndexI dim)

infixr 5 :.=

-- | A class used both as a shorthand for useful @`Index`@ instances and a way to obtain
-- a value of @`IndexI`@.
class (Bounded (Index dim), Enum (Index dim)) => Ix dim where
  -- | Returns a term-level witness of @Ix@.
  inst :: IndexI dim

instance Ix '[] where
  inst = EI

instance (KnownNat n, 1 <= n, Ix ds) => Ix (n:ds) where
  inst = Proxy :.= inst

-- | Convert @`Sing`@ of a
singToIndexI :: forall dim . Sing dim -> Maybe (IndexI dim)
singToIndexI (SCons sn@SNat SNil) =
  case sing @1 %<=? sn of
    STrue  -> Just II
    SFalse -> Nothing
singToIndexI (SCons sn@SNat sr@SCons{}) =
  case (sing @1 %<=? sn, singToIndexI sr) of
    (STrue, Just r@IxI) -> Just $ II :.= r
    _                   -> Nothing
singToIndexI _ = Nothing

withIx :: Demote [Natural] -> (forall dim . Ix dim => Proxy dim -> r) -> Maybe r
withIx d f = withSomeSing d \(singToIndexI -> r) ->
  flip fmap r \case (IxI :: IndexI dim) -> f $ Proxy @dim
