{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : MLambda.NDArr
-- Description : Basic ndarray type.
-- Copyright   : (c) neclitoris, TurtlePU, 2025
-- License     : BSD-3-Clause
-- Maintainer  : nas140301@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module contains definition of 'NDArr' type of multidimensional arrays
-- along with its instances and generic public interface.
--
-- For matrix operations, take a look at 'MLambda.Matrix'.
-- For definition of 'Index' type, see 'MLambda.Index'.
module MLambda.NDArr
  ( NDArr
  , runNDArr
  -- * Array creation
  , fromIndex
  , fromIndexM
  -- * Array access
  , at
  , row
  , rows
  -- * Array operations
  , toList
  , concat
  , map
  , zipWith
  , foldr
  -- * Array composition
  , Stack
  , Stacks
  , StackWitness(..)
  , stack
  , stackWithWitness
  -- * Unsafe API
  , unsafeMkNDArr
  -- * Shape manipulation
  , reshape
  , prependDim
  , stripDim
  ) where

import MLambda.Index
import MLambda.Linear
import MLambda.TypeLits

import Control.DeepSeq (NFData)
import Control.Monad.ST (runST)
import Data.Foldable (forM_)
import Data.List qualified as List
import Data.List.Singletons
import Data.Singletons
import Data.Vector.Storable qualified as Storable
import Data.Vector.Storable.Mutable qualified as Mutable
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (..))
import GHC.TypeError (ErrorMessage (..), TypeError)
import Prelude hiding (concat, foldr, map, zipWith)

-- | @NDArr [n1,...,nd] e@ is a type of arrays with dimensions @n1 x ... x nd@
-- consisting of elements of type @e@.
newtype NDArr (dim :: [Natural]) e = MkNDArr {
    -- | Raw data of an array in row-first order.
    -- Length of data for @NDArr [n1,...,nd]@ is guaranteed to be @n1 x ... x nd@.
    runNDArr :: Storable.Vector e
  }
  deriving (Eq, NFData)

-- | Unsafe O(1) constructor of new multidimensional arrays from row-first raw data.
-- Unsafe because size is not checked.
unsafeMkNDArr :: forall dim e. Storable.Vector e -> NDArr dim e
unsafeMkNDArr = MkNDArr

instance (Ix dim, Show e, Storable e) => Show (NDArr dim e) where
  showsPrec _ =
    case inst @dim of
      _ :.= _ -> go
      EI      -> shows . (Storable.! 0) . runNDArr
    where
      go :: forall n r e' . (Ix r, Show (NDArr r e'), Storable e') => NDArr (n:r) e' -> ShowS
      go = (showString "[" .) . (. showString "]")
          . foldl' (.) id
          . List.intersperse (showString ",\n")
          . List.map shows
          . toList . rows @'[n]

instance (Ix d, Storable e) => Storable (NDArr d e) where
  sizeOf _ = sizeOf (undefined :: e) * enumSize (Index d)
  alignment _ = alignment (undefined :: e)
  peek (castPtr -> (ptr :: Ptr e)) = fromIndexM (peekElemOff ptr . fromEnum)
  poke (castPtr -> (ptr :: Ptr e)) = (`Storable.iforM_` pokeElemOff ptr) . runNDArr

instance (Ix d, Storable r, Num r) => Additive (NDArr d r) where
  add = zipWith (+)
  zero = MkNDArr $ Storable.replicate (enumSize (Index d)) 0

instance (Ix d, Storable r, Num r) => Module r (NDArr d r) where
  modMult r = MkNDArr . Storable.map (r *) . runNDArr

-- | Construct an array from a function @f@ that maps indices to elements.
-- This function is strict and therefore @f@ can't refer to the result of
-- @fromIndex f@.
--
-- Property:
--
-- prop> fromIndex f `at` i == f i
fromIndex :: forall dim e . (Ix dim, Storable e) => (Index dim -> e) -> NDArr dim e
fromIndex f = runST $ fromIndexM $ pure . f

-- | Same as @`fromIndex`@, but monadic.
fromIndexM :: forall dim m e . (Mutable.PrimMonad m, Ix dim, Storable e)
           => (Index dim -> m e) -> m (NDArr dim e)
fromIndexM f = do
  mvec <- Mutable.new (enumSize (Index dim))
  loop_ (\i -> f i >>= Mutable.write mvec (fromEnum i))
  vec <- Storable.unsafeFreeze mvec
  pure $ MkNDArr vec

-- | O(1). Access array element by its index. This is a total function.
at :: (Storable e, Ix dim) => NDArr dim e -> Index dim -> e
(MkNDArr v) `at` i = v Storable.! fromEnum i

infixl 9 `at`

-- | O(1). Extract a "row" from the array. If you're used to C or numpy arrays,
-- this is similar to @a[i]@.
row ::
  forall d1 d2 e. (Ix d1, Ix d2, Storable e) =>
  Index d1 -> NDArr (d1 ++ d2) e -> NDArr d2 e
row i a = rows a `at` i

-- | O(1). Extract all "rows" from the array as an array.
rows ::
  forall d1 d2 e. (Ix d2, Storable e) =>
  NDArr (d1 ++ d2) e -> NDArr d1 (NDArr d2 e)
rows = MkNDArr . Storable.unsafeCast . runNDArr

-- | O(array_size). Extract @`NDArr`@ elements as a list in the order they are
-- laid out in memory.
toList :: Storable e => NDArr d e -> [e]
toList = Storable.toList . runNDArr

-- | O(1). Flatten an array of arrays into a single array.
concat ::
  forall d1 d2 e. (Ix d2, Storable e) =>
  NDArr d1 (NDArr d2 e) -> NDArr (d1 ++ d2) e
concat = MkNDArr . Storable.unsafeCast . runNDArr

-- | O(array_size). Transform elements of an array.
map :: (Storable a, Storable b) => (a -> b) -> NDArr d a -> NDArr d b
map f = MkNDArr . Storable.map f . runNDArr

-- | O(array_size). Combine two arrays into one element-by-element.
zipWith ::
  (Storable a, Storable b, Storable c) =>
  (a -> b -> c) -> NDArr d a -> NDArr d b -> NDArr d c
zipWith f (MkNDArr xs) (MkNDArr ys) = MkNDArr (Storable.zipWith f xs ys)

-- | O(array_size). Reduce array using an accumulator function and initial value.
foldr :: Storable a => (a -> r -> r) -> r -> NDArr d a -> r
foldr f r = Storable.foldr f r . runNDArr

vstack ::
  Storable e => NDArr (k : d) e -> NDArr (l : d) e -> NDArr ((k + l) : d) e
vstack (MkNDArr xs) (MkNDArr ys) = MkNDArr (xs <> ys)

-- | A type family which computes the resulting size of a stacked array.
type Stack n d e = StackImpl (StackError n d e) n d e

type StackError n d e =
  Text "Not enough dimensions to stack along axis " :<>: ShowType n
  :<>: Text ":" :$$: ShowType d :$$: ShowType e

type family StackImpl msg i d e where
  StackImpl _ PZ (n : d) (m : e) = n + m : Unify "Dimensions" d e
  StackImpl msg (PS i) (n : d) (m : e) = Unify "Sizes" n m : StackImpl msg i d e
  StackImpl msg _ _ _ = TypeError msg

-- | A constraint which links together compatible dimensions and axis along
-- which they will be stacked.
class Stacks i dim1 dim2 dimr where
  stacks :: StackWitness i dim1 dim2 dimr

-- | A witness that carries the constraints needed to stack arrays together.
data StackWitness i d1 d2 dr where
  SW ::
    ( KnownNat k, KnownNat l, KnownNat (k + l), Ix t
    , 1 <= k, 1 <= l, 1 <= k + l
    ) => Proxy '(s, k, l, t) ->
    StackWitness (PLength s) (s ++ (k : t)) (s ++ (l : t)) (s ++ ((k + l) : t))

instance
  ( KnownNat n, KnownNat m, KnownNat k, Ix d
  , n + m ~ k, 1 <= n, 1 <= m, 1 <= k
  ) => Stacks PZ (n : d) (m : d) (k : d) where
  stacks = SW (Proxy @' ('[], n, m, d))

instance Stacks i d e r => Stacks (PS i) (n : d) (n : e) (n : r) where
  stacks = case stacks @i of
    SW (Proxy @'(s, k, l, t)) -> SW (Proxy @'(n : s, k, l, t))

-- | Sometimes you need to create a witness yourself for this to work.
stackWithWitness :: Storable e => StackWitness n d1 d2 dr
                 -> NDArr d1 e -> NDArr d2 e -> NDArr dr e
stackWithWitness (SW (Proxy @'(s, k, l, t))) xs ys =
  concat $ zipWith vstack (rows @s @(k : t) xs) (rows @s @(l : t) ys)

-- | @stack i@ stacks arrays along the axis @i@. All other axes are required
-- to be the same lengths.
stack ::
  forall n -> (Stacks (Peano n) d1 d2 (Stack (Peano n) d1 d2), Storable e) =>
  NDArr d1 e -> NDArr d2 e -> NDArr (Stack (Peano n) d1 d2) e
stack n = stackWithWitness (stacks @(Peano n))

type family Size d where
  Size '[]    = 1
  Size (x:xs) = x * Size xs

-- | Change the shape of an array.
reshape :: forall d2 -> (Size d1 ~ Size d2) => NDArr d1 e -> NDArr d2 e
reshape _ = MkNDArr . runNDArr

-- | Prepend a single dimension of size 1.
prependDim :: NDArr d e -> NDArr (1:d) e
prependDim = MkNDArr . runNDArr

-- | Remove a dimension of size 1 from the front.
stripDim :: NDArr (1:d) e -> NDArr d e
stripDim = MkNDArr . runNDArr
