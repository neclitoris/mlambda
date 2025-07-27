-- |
-- Module      : MLambda.Matrix
-- Description : Basic ndarray type.
-- Copyright   : (c) neclitoris, 2025
-- License     : BSD-3-Clause
-- Maintainer  : nas140301@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module contains definition of 'NDArr' type of multidimensional arrays
-- along with its instances and public interface.
module MLambda.Matrix
  ( NDArr(..)
  , cross
  ) where

import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as V
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Storable
import GHC.IO
import GHC.TypeLits
import Numeric.BLAS.FFI.Double

-- | 'NDArr [n1,...nd] e' is a type of arrays with dimensions @n1 x ... x nd@
-- consisting of elements of type @e@.
newtype NDArr (dim :: [Natural]) e = MkNDArr (Vector e)

deriving instance (Eq e, Storable e) => Eq (NDArr dim e)
deriving instance (Show e, Storable e) => Show (NDArr dim e)

-- | Your usual matrix product. Calls into BLAS's @gemm@ operation.
cross :: forall n m k . (KnownNat n, KnownNat m, KnownNat k)
    => NDArr [m, k] Double -> NDArr [k, n] Double -> NDArr [m, n] Double
MkNDArr a `cross` MkNDArr b = unsafePerformIO $ do
  let (afptr, _alen) = V.unsafeToForeignPtr0 a
      (bfptr, _blen) = V.unsafeToForeignPtr0 b
      m = fromInteger $ fromSNat (SNat @m)
      n = fromInteger $ fromSNat (SNat @n)
      _k :: Int = fromInteger $ fromSNat (SNat @k)
  cfptr <- mallocForeignPtrBytes (m * n)
  -- Fortran (and BLAS) uses column-major indexing,
  -- so we switch the inputs order.
  withCString "N" \mode ->
    withForeignPtr afptr \aptr ->
      withForeignPtr bfptr \bptr ->
        withForeignPtr cfptr \cptr ->
          with (fromInteger $ fromSNat (SNat @m)) \mptr ->
            with (fromInteger $ fromSNat (SNat @n)) \nptr ->
              with (fromInteger $ fromSNat (SNat @k)) \kptr ->
                with 1 \alpha ->
                  with 0 \beta ->
                    gemm mode mode nptr mptr kptr alpha
                      bptr nptr
                      aptr kptr
                      beta cptr nptr
  let carr = V.unsafeFromForeignPtr0 cfptr (m * n)
  pure $ MkNDArr carr
