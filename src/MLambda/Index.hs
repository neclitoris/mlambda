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
  ( Index ((:.))
  , consIndex
  , concatIndex
  , IndexI (..)
  , Ix
  , inst
  ) where

import MLambda.TypeLits

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
  (:.) :: Index '[n] -> Index (d : ds) -> Index (n : d : ds)
  I :: Int -> Index '[n]

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

instance (KnownNat n, 1 <= n) => Bounded (Index '[n]) where
  minBound = I 0
  maxBound = I $ natVal n - 1

instance (KnownNat n, 1 <= n, Bounded (Index (a:r))) => Bounded (Index (n:a:r)) where
  minBound = minBound :. minBound
  maxBound = maxBound :. maxBound

instance (KnownNat n, 1 <= n) => Enum (Index '[n]) where
  fromEnum (I m) = m
  toEnum = I . (`mod` natVal n)
  succ (I m) | m == natVal n - 1 = error "Undefined succ"
  succ (I m) = I (m + 1)
  pred (I 0) = error "Undefined pred"
  pred (I m) = I (m - 1)

instance (KnownNat n, 1 <= n, Enum (Index (a:r)), Bounded (Index (a:r)))
  => Enum (Index (n:a:r)) where
  fromEnum (I n :. t) = enumSize (Index (a:r)) * n + fromEnum t
  toEnum m = I q :. toEnum t
    where
      (q, t) = m `quotRem` enumSize (Index (a:r))
  succ (h :. t) | t == maxBound = succ h :. minBound
  succ (h :. t) = h :. succ t
  pred (h :. t) | t == minBound = pred h :. maxBound
  pred (h :. t) = h :. pred t

-- | Prepend a single-dimensional index to multi-dimensional one
consIndex :: Index '[x] -> Index xs -> Index (x : xs)
consIndex (I x) = \case
  I y -> I x :. I y
  I y :. xs -> I x :. I y :. xs

-- | Concatenate two indices together
concatIndex :: Index xs -> Index ys -> Index (xs ++ ys)
concatIndex (I x) = consIndex (I x)
concatIndex (I x :. xs) = consIndex (I x) . concatIndex xs

-- | A helper type that holds instances for everything you can get by pattern matching on
-- an @`Index`@ value. If you need to get instances for a head/tail of an @`Index`@,
-- pattern matching on a value of this type will bring them into the scope.
--
-- > f :: Ix (d : ds) => Index (d : ds) -> r
-- > f (i :. j) = case inst @(d : ds) of
-- >                _ :.= _ -> f j  -- @f j@ can be called here because we have @Ix ds@ now
-- > f i        = ...
data IndexI (dim :: [Natural]) where
  (:.=) :: Ix (d : ds) => IndexI '[n] -> IndexI (d : ds) -> IndexI (n : d : ds)
  II :: (KnownNat n, 1 <= n) => IndexI '[n]

infixr 5 :.=

-- | A class used both as a shorthand for useful @`Index`@ instances and a way to obtain
-- a value of @`IndexI`@.
class (Bounded (Index dim), Enum (Index dim)) => Ix dim where
  -- | Returns a term-level witness of @Ix@.
  inst :: IndexI dim

instance (KnownNat n, 1 <= n) => Ix '[n] where
  inst = II

instance (KnownNat n, 1 <= n, Ix (d:ds)) => Ix (n:d:ds) where
  inst = II :.= inst
