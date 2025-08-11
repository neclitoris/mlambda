{-# LANGUAGE RequiredTypeArguments #-}

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
  -- * Array composition
  , Stack
  , stack
  -- * Unsafe API
  , unsafeMkNDArr
  ) where

import MLambda.Index
import MLambda.TypeLits

import Control.DeepSeq (NFData)
import Control.Monad.ST (runST)
import Data.Foldable (forM_)
import Data.List (intersperse)
import Data.Vector.Storable qualified as Storable
import Data.Vector.Storable.Mutable qualified as Mutable
import Foreign.Storable (Storable)
import GHC.TypeError (ErrorMessage (..), TypeError)

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

instance (Show e, Storable e) => Show (NDArr '[n] e) where
  show = show . runNDArr

instance (Ix (n:a:r), Show (NDArr (a:r) e), Storable e) =>
  Show (NDArr (n:a:r) e) where
  showsPrec _ =
    case inst @(n:a:r) of
      II :.= _ ->
        (showString "[" .) . (. showString "]")
        . foldl' (.) id
        . intersperse (showString ",\n")
        . map (shows . rows) . (:[])

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
  forM_ [minBound..maxBound] (\i -> f i >>= Mutable.write mvec (fromEnum i))
  vec <- Storable.unsafeFreeze mvec
  pure $ MkNDArr vec

-- | Access array element by its index. This is a total function.
at :: (Storable e, Ix dim) => NDArr dim e -> Index dim -> e
(MkNDArr v) `at` i = v Storable.! fromEnum i

infixl 9 `at`

-- | Extract a "row" from the array. If you're used to C or numpy arrays,
-- this is similar to @a[i]@.
row :: forall r e {n} {a} {b} .
    ( Ix (n:r)
    , r ~ (a:b)
    , Storable e)
    => Index '[n] -> NDArr (n:r) e -> NDArr r e
row i =
  case inst @(n:r) of
    II :.= _ -> let i' = i :. (minBound :: Index r)
                 in MkNDArr
                  . Storable.slice (fromEnum i') (enumSize (Index r))
                  . runNDArr

-- | Extract all "rows" from the array as a list.
rows :: forall r e {n} {a} {b} .
     ( Ix (n:r)
     , r ~ (a:b)
     , Storable e)
     => NDArr (n:r) e -> [NDArr r e]
rows a = case inst @(n:r) of
           II :.= _ -> [row i a | i <- [minBound..maxBound]]

-- | @Stack i d1 d2@ are dimensions of an array which is a result of stacking
-- arrays of dimensions @d1@ and @d2@ along axis @i@.
type family Stack i d1 d2 where
  Stack 0 (n : d1) (m : d2) = n + m : Unify "Dimensions" d1 d2
  Stack i (n : d1) (m : d2) = Unify "Sizes" n m : Stack (i - 1) d1 d2
  Stack i d1 d2 = TypeError (
    Text "Not enough dimensions to stack along axis "
    :<>: ShowType i :<>: Text ":" :$$: ShowType d1 :$$: ShowType d2)

-- | @stack i@ stacks arrays along the axis @i@. All other axes are required
-- to be the same lengths.
stack ::
  forall n -> (KnownNat n, Ix d1, Ix d2, Ix (Stack n d1 d2), Storable e) =>
  NDArr d1 e -> NDArr d2 e -> NDArr (Stack n d1 d2) e
stack n = \(a :: NDArr d1 e) (b :: NDArr d2 e) ->
  inst @(Stack n d1 d2) `seq` MkNDArr $ go (natVal n) inst inst a b
  where
    go ::
      Storable e => Natural -> IndexI d1 -> IndexI d2 ->
      NDArr d1 e -> NDArr d2 e -> Storable.Vector e
    go 0 _ _ = \a b -> Storable.concat [runNDArr a, runNDArr b]
    go r (II :.= i) (II :.= j) = \a b ->
      Storable.concat $ zipWith (go (r - 1) i j) (rows a) (rows b)
    go _ _ _ = error "stack: impossible"
