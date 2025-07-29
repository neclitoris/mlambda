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

import Control.Monad.Cont
import Control.Monad.IO.Class
import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as V
import Foreign.ForeignPtr
import Foreign.Storable
import GHC.IO hiding (liftIO)
import GHC.TypeLits
import Numeric.BLAS.FFI.Double

import MLambda.Foreign.Utils

-- | 'NDArr [n1,...nd] e' is a type of arrays with dimensions @n1 x ... x nd@
-- consisting of elements of type @e@.
newtype NDArr (dim :: [Natural]) e = MkNDArr (Vector e)

deriving instance (Eq e, Storable e) => Eq (NDArr dim e)
deriving instance (Show e, Storable e) => Show (NDArr dim e)

-- | Your usual matrix product. Calls into BLAS's @gemm@ operation.
cross :: forall n m k . (KnownNat n, KnownNat m, KnownNat k)
    => NDArr [m, k] Double -> NDArr [k, n] Double -> NDArr [m, n] Double
MkNDArr a `cross` MkNDArr b = unsafePerformIO . evalContT $ do
  let (afptr, _alen) = V.unsafeToForeignPtr0 a
      (bfptr, _blen) = V.unsafeToForeignPtr0 b
      m = fromSNat (SNat @m)
      n = fromSNat (SNat @n)
      k = fromSNat (SNat @k)
      len = fromInteger $ m * n
  cfptr <- liftIO $ mallocForeignPtrBytes len
  -- Fortran (and BLAS) uses column-major indexing,
  -- so we switch the inputs order.
  mode <- char 'N'
  aptr <- asFPtr afptr
  bptr <- asFPtr bfptr
  cptr <- asFPtr cfptr
  mptr <- asPtr $ fromInteger m
  nptr <- asPtr $ fromInteger n
  kptr <- asPtr $ fromInteger k
  alpha <- asPtr 1
  beta <- asPtr 0
  _ <- pure $
    gemm mode mode nptr mptr kptr alpha
      bptr nptr
      aptr kptr
      beta cptr nptr
  let carr = V.unsafeFromForeignPtr0 cfptr len
  pure $ MkNDArr carr
