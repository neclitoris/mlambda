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
  ( NDArr (..)
  , cross
  , crossMassiv
  , mat
  ) where

import MLambda.Foreign.Utils (asFPtr, asPtr, char)
import MLambda.TypeLits

import Control.DeepSeq (NFData)
import Control.Exception (assert)
import Control.Monad
import Control.Monad.Cont
import Control.Monad.IO.Class
import Data.Massiv.Array qualified as Massiv
import Data.Massiv.Array.Manifest.Vector qualified as Massiv
import Data.Vector.Storable qualified as Storable
import Data.Vector.Storable qualified as V
import Foreign.ForeignPtr
import Foreign.Storable
import GHC.IO hiding (liftIO)
import GHC.Ptr
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.CodeDo qualified as Code
import Language.Haskell.TH.Quote qualified as Quote
import Numeric.BLAS.FFI.Double

-- | 'NDArr [n1,...nd] e' is a type of arrays with dimensions @n1 x ... x nd@
-- consisting of elements of type @e@.
newtype NDArr (dim :: [Natural]) e = MkNDArr { runNDArr :: Storable.Vector e }
  deriving (Eq, Show, NFData)

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
crossMassiv a b = fromMassiv (toMassiv a Massiv.!><! toMassiv b)

-- | Your usual matrix product. Calls into BLAS's @gemm@ operation.
cross :: forall n m k . (KnownNat n, KnownNat m, KnownNat k)
    => NDArr [m, k] Double -> NDArr [k, n] Double -> NDArr [m, n] Double
MkNDArr a `cross` MkNDArr b = unsafePerformIO . evalContT $ do
  let (afptr, _alen) = V.unsafeToForeignPtr0 a
      (bfptr, _blen) = V.unsafeToForeignPtr0 b
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
  let carr = V.unsafeFromForeignPtr0 cfptr len
  return (MkNDArr carr)

-- | `mat` is a quasi-quote used to safely define constant matrices.
-- Usage:
-- > $$[mat| 1 2 3
-- >         4 5 6 |]
-- This quote will fail if type of the expression it appears in
-- is not a 2x3 matrix.
mat :: Quote.QuasiQuoter
mat = Quote.QuasiQuoter
  { quoteExp = \s -> [| matT s |]
  , quotePat = error "Not implemented"
  , quoteType = error "Not implemented"
  , quoteDec = error "Not implemented"
  }

matT :: forall e m n .
  ( Storable e
  , Read e
  , KnownNat m
  , KnownNat n
  ) => String -> TH.CodeQ (NDArr [m, n] e)
matT s = Code.do
  let l = map (map (read @e) . words)
        $ takeWhile (not . null)
        $ dropWhile null
        $ lines s
  when (length l /= natVal m) $ fail "Wrong row count"
  when (any ((/= natVal n) . length) l) $ fail "Wrong column count"
  let dat = V.fromList $ concat l
      (fptr, offset, len) = V.unsafeToForeignPtr $ V.unsafeCast dat
      bytes = TH.unsafeCodeCoerce
            $ TH.litE $ TH.bytesPrimL
            $ TH.mkBytes fptr (fromIntegral offset) (fromIntegral len)
      size = natVal m * natVal n
  [||
    unsafePerformIO $ do
      ptr <- newForeignPtr_ (Ptr $$bytes)
      return $ MkNDArr (V.unsafeFromForeignPtr0 ptr size)
    ||]
