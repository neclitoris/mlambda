{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RequiredTypeArguments #-}
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
  ( -- * Index type
    Index (E)
  , pattern (:.)
  -- * Index instances
  , Ix(..)
  , IndexI (..)
  , pattern IxI
  , concatIndexI
  -- * Lifting of runtime dimesions into indices
  , singToIndexI
  , withIx
  -- * Index operations
  , concatIndex
  -- * Efficient iteration
  , enumerate
  , loop_
  ) where

import MLambda.TypeLits

import Control.Monad
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
  ICons :: Int -> Index ds -> Index (d : ds)

{-# COMPLETE I #-}
pattern I :: Int -> Index '[d]
pattern I m = ICons m E

viewI :: Index (d:ds) -> (Index '[d], Index ds)
viewI (ICons x xs) = (I x, xs)

{-# COMPLETE (:.) #-}
-- | Prepend a single-dimensional index to multi-dimensional one
pattern (:.) :: Index '[d] -> Index ds -> Index (d:ds)
pattern x :. xs <- (viewI -> (x, xs))
  where (ICons x E) :. xs = ICons x xs

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
  minBound = 0    :. minBound
  maxBound = (-1) :. maxBound

instance Enum (Index '[]) where
  toEnum = const E
  fromEnum = const 0

instance (KnownNat n, 1 <= n, Enum (Index d), Bounded (Index d)) =>
  Enum (Index (n:d)) where
  toEnum ((`quotRem` enumSize (Index d)) -> (q, r)) = I (q `mod` natVal n) :. toEnum r
  fromEnum (I q :. r) = q * enumSize (Index d) + fromEnum r
  succ (h :. t) | t == maxBound = succ h :. minBound
                | otherwise     = h :. succ t
  pred (h :. t) | t == minBound = pred h :. maxBound
                | otherwise     = h :. pred t

-- | Efficiently (compared to @`enumFromTo`@) enumerate all indices in lexicographic
-- order.
enumerate :: forall d -> Ix d => [Index d]
enumerate d =
  case IxI @d of
    EI           -> [E]
    -- TODO: this performs very poorly, optimize
    _ :.= IxI @r -> (:.) <$> [minBound..maxBound] <*> enumerate r

-- | Efficiently iterate through indices in lexicographic order.
loop_ :: forall d m e . (Ix d, Monad m) => (Index d -> m e) -> m ()
loop_ = forM_ (enumerate d)

-- | Concatenate two indices together
concatIndex :: forall xs ys . Index xs -> Index ys -> Index (xs ++ ys)
concatIndex E            = id
concatIndex (ICons x xs) = ICons x . concatIndex xs

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
viewII (_ :.= _) = IxInstance
viewII EI        = IxInstance

-- | A simpler pattern for @`IndexI`@ in case you don't need instances
-- for suffixes.
{-# COMPLETE IxI #-}
pattern IxI :: () => Ix dim => IndexI dim
pattern IxI <- (viewII -> IxInstance) where
  IxI = inst

deriving instance Show (IndexI dim)

infixr 5 :.=

-- | Concatenate two @`IndexI`@s to get the instances for concatenated
-- dimensions.
concatIndexI :: IndexI d1 -> IndexI d2 -> IndexI (d1 ++ d2)
concatIndexI EI d2          = d2
concatIndexI (i1 :.= d1) d2 = case concatIndexI d1 d2 of
                                IxI -> i1 :.= IxI

-- | A class used both as a shorthand for useful @`Index`@ instances and a way to obtain
-- a value of @`IndexI`@.
class (Bounded (Index dim), Enum (Index dim)) => Ix dim where
  -- | Returns a term-level witness of @Ix@.
  inst :: IndexI dim

instance Ix '[] where
  inst = EI

instance (KnownNat n, 1 <= n, Ix ds) => Ix (n:ds) where
  inst = Proxy :.= inst

-- | Convert @`Sing`@ of a dimension list to a witness for @`Ix`@ instances
-- of that index.
singToIndexI :: forall dim . Sing dim -> Maybe (IndexI dim)
singToIndexI SNil = Just EI
singToIndexI (SCons sn@SNat sr) =
  case (sing @1 %<=? sn, singToIndexI sr) of
    (STrue, Just r@IxI) -> Just $ Proxy :.= r
    _                   -> Nothing

-- | Simple interface for lifting runtime dimensions into type level.
-- Provides the given continuation with the type of said dimensions and their
-- @`Ix`@ instance.
withIx :: Demote [Natural] -> (forall dim . Ix dim => Proxy dim -> r) -> Maybe r
withIx d f = withSomeSing d \(singToIndexI -> r) ->
  flip fmap r \case (IxI :: IndexI dim) -> f $ Proxy @dim
