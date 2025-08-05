{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : MLambda.Matrix
-- Description : Basic ndarray type.
-- Copyright   : (c) neclitoris, TurtlePU, 2025
-- License     : BSD-3-Clause
-- Maintainer  : nas140301@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module contains definition of 'NDArr' type of multidimensional arrays
-- along with its instances and public interface.
module MLambda.Matrix
  ( NDArr(..)
  , Index((:.))
  -- * Matrix operations
  , cross
  , crossMassiv
  -- * Array creation
  , mat
  , fromIndex
  -- * Array access
  , at
  , row
  ) where

import MLambda.Foreign.Utils (asFPtr, asPtr, char)
import MLambda.TypeLits

import Control.DeepSeq (NFData)
import Control.Exception (assert)
import Control.Monad
import Control.Monad.Cont
import Control.Monad.IO.Class
import Control.Monad.ST (runST)
import Data.List (intersperse)
import Data.Massiv.Array qualified as Massiv
import Data.Massiv.Array.Manifest.Vector qualified as Massiv
import Data.Maybe
import Data.Vector.Storable qualified as Storable
import Data.Vector.Storable.Mutable qualified as Mutable
import Foreign.ForeignPtr
import Foreign.Storable
import GHC.IO hiding (liftIO)
import GHC.Ptr
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote qualified as Quote
import Numeric.BLAS.FFI.Double

-- | `NDArr [n1,...nd] e` is a type of arrays with dimensions @n1 x ... x nd@
-- consisting of elements of type @e@.
newtype NDArr (dim :: [Natural]) e = MkNDArr { runNDArr :: Storable.Vector e }
  deriving (Eq, NFData)

instance (Show e, Storable e) => Show (NDArr '[n] e) where
  show = show . runNDArr

instance (KnownNat n, Enum (Index (a:r)), Bounded (Index (a:r))
         , Show (NDArr (a:r) e), Storable e) => Show (NDArr (n:a:r) e) where
  showsPrec _ = (showString "[" .) . (. showString "]")
    . foldl' (.) id
    . intersperse (showString ",\n")
    . map shows
    . ([row i | i <- [0..maxBound]] <*>)
    . (:[])

massivSize :: forall m n -> (KnownNat m, KnownNat n) => Massiv.Sz2
massivSize m n = natVal m `Massiv.Sz2` natVal n

fromMassiv ::
  forall m n e. (KnownNat m, KnownNat n, Storable e) =>
  Massiv.Array Massiv.S Massiv.Ix2 e -> NDArr [m, n] e
fromMassiv arr =
  MkNDArr $ Massiv.toVector $ assert (Massiv.size arr == massivSize m n) arr

toMassiv ::
  forall m n e. (KnownNat m, KnownNat n, Storable e) =>
  NDArr [m, n] e -> Massiv.Array Massiv.S Massiv.Ix2 e
toMassiv = Massiv.fromVector' Massiv.Par (massivSize m n) . runNDArr

-- | Matrix product reused from massiv.
crossMassiv ::
  (KnownNat m, KnownNat k, KnownNat n) =>
  NDArr [m, k] Double -> NDArr [k, n] Double -> NDArr [m, n] Double
crossMassiv a b = fromMassiv $ toMassiv a Massiv.!><! toMassiv b

-- | Your usual matrix product. Calls into BLAS's @gemm@ operation.
cross :: forall n m k . (KnownNat n, KnownNat m, KnownNat k)
    => NDArr [m, k] Double -> NDArr [k, n] Double -> NDArr [m, n] Double
MkNDArr a `cross` MkNDArr b = unsafePerformIO $ evalContT do
  let (afptr, _alen) = Storable.unsafeToForeignPtr0 a
      (bfptr, _blen) = Storable.unsafeToForeignPtr0 b
      len = natVal m * natVal n
  cfptr <- liftIO $ mallocForeignPtrArray len
  -- Fortran (and BLAS) uses column-major indexing,
  -- so we switch the inputs order.
  mode <- char 'N'
  aptr <- asFPtr afptr
  bptr <- asFPtr bfptr
  cptr <- asFPtr cfptr
  mptr <- asPtr $ natVal m
  nptr <- asPtr $ natVal n
  kptr <- asPtr $ natVal k
  alpha <- asPtr 1
  beta <- asPtr 0
  liftIO $ gemm mode mode nptr mptr kptr alpha
      bptr nptr
      aptr kptr
      beta cptr nptr
  let carr = Storable.unsafeFromForeignPtr0 cfptr len
  pure $ MkNDArr carr

infixl 7 `cross`
infixl 7 `crossMassiv`

-- | `mat` is a quasi-quote used to safely define constant matrices.
-- Usage:
-- > $$[mat| 1 2 3
-- >         4 5 6 |]
-- This quote will fail if type of the expression it appears in
-- is not a 2x3 matrix.
mat :: Quote.QuasiQuoter
mat = Quote.QuasiQuoter
  { quoteExp = matE
  , quotePat = error "Not implemented"
  , quoteType = error "Not implemented"
  , quoteDec = error "Not implemented"
  }

matE :: (MonadFail m, TH.Quote m) => String -> m TH.Exp
matE s = do
  let l = map (map (read @Double) . words)
        $ takeWhile (not . null)
        $ dropWhile null
        $ lines s
  let m  = length l
      tm = TH.litT $ TH.numTyLit $ toInteger m
  n <- maybe (fail "Empty matrix specified") (pure . length) $ listToMaybe l
  let tn = TH.litT $ TH.numTyLit $ toInteger n
  when (n == 0) $ fail "Empty matrix specified"
  when (any ((/= n) . length) l) $ fail "Wrong column count"
  let dat = Storable.fromList $ concat l
      (fptr, offset, len) =
        Storable.unsafeToForeignPtr $ Storable.unsafeCast dat
      bytes = TH.litE $ TH.bytesPrimL
            $ TH.mkBytes fptr (fromIntegral offset)
            $ fromIntegral len
      size = m * n
  [|
    unsafePerformIO do
      ptr <- newForeignPtr_ $ Ptr $bytes
      pure $ MkNDArr @'[$tm, $tn] @Double $ Storable.unsafeFromForeignPtr0 ptr size
    |]

-- | `Index dim` is the type of indices of an array of type `NDArr dim e`.
data Index (dim :: [Natural]) where
  (:.) :: Index '[n] -> Index d -> Index (n : d)
  I :: Int -> Index '[n]

deriving instance Eq (Index dim)
deriving instance Ord (Index dim)
deriving instance Show (Index dim)

{-# COMPLETE (:.) #-}
{-# COMPLETE I #-}

infixr 5 :.

instance KnownNat n => Num (Index '[n]) where
  fromInteger = I . (`mod` natVal n) . fromInteger
  abs = id
  signum _ = 1
  negate (I m) = I $ natVal n - 1 - m
  (I a) + (I b) = I $ (a + b) `mod` natVal n
  (I a) * (I b) = I $ (a * b) `mod` natVal n

instance KnownNat n => Bounded (Index '[n]) where
  minBound = I 0
  maxBound = I $ natVal n - 1

instance (KnownNat n, Bounded (Index (a:r))) => Bounded (Index (n:a:r)) where
  minBound = I 0 :. minBound
  maxBound = I (natVal n - 1) :. maxBound

instance KnownNat n => Enum (Index '[n]) where
  fromEnum (I m) = m
  toEnum = I . (`mod` natVal n)
  succ (I m) | m == natVal n - 1 = error "Undefined succ"
  succ (I m) = I (m + 1)
  pred (I 0) = error "Undefined pred"
  pred (I m) = I (m - 1)

instance (KnownNat n, Enum (Index (a:r)), Bounded (Index (a:r))) => Enum (Index (n:a:r)) where
  fromEnum (I n :. t) = enumSize (Index (a:r)) * n + fromEnum t
  toEnum m = I q :. toEnum t
    where
      (q, t) = m `quotRem` enumSize (Index (a:r))
  succ (h :. t) | t == maxBound = succ h :. minBound
  succ (h :. t) = h :. succ t
  pred (h :. t) | t == minBound = pred h :. maxBound
  pred (h :. t) = h :. pred t

-- | Access array element by its index. This is a total function.
at :: (Storable e, Enum (Index dim)) => NDArr dim e -> Index dim -> e
(MkNDArr v) `at` i = v Storable.! fromEnum i

-- | Construct an array from a function `f` that maps indices to elements.
-- This function is strict and therefore `f` can't refer to the result of
-- `fromFunction f`.
fromIndex :: forall dim e . (Enum (Index dim), Bounded (Index dim), Storable e)
          => (Index dim -> e) -> NDArr dim e
fromIndex f = runST do
  mvec <- Mutable.new (enumSize (Index dim))
  forM_ [minBound..maxBound] (\i -> Mutable.write mvec (fromEnum i) (f i))
  vec <- Storable.unsafeFreeze mvec
  pure $ MkNDArr vec

-- | Extract a "row" from the array. If you're used to C or numpy arrays,
-- this is similar to a[i].
row :: forall n r e {a} {b} .
    ( KnownNat n
    , Enum (Index r)
    , Bounded (Index r)
    , r ~ (a:b)
    , Storable e)
    => Index '[n] -> NDArr (n:r) e -> NDArr r e
row i = let i' = (i :. minBound) :: Index (n:r)
         in MkNDArr . Storable.slice (fromEnum i') (enumSize (Index r)) . runNDArr
