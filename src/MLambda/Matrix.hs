module MLambda.Matrix
  ( NDArr(..)
  , (@)
  ) where

import Data.Vector.Storable qualified as V
import Data.Vector.Storable (Vector)
import Foreign.C.String
import Foreign.Storable
import Foreign.Marshal.Utils
import Foreign.ForeignPtr
import Foreign.Ptr
import GHC.IO
import GHC.TypeLits
import Numeric.BLAS.FFI.Double

newtype NDArr (dim :: [Natural]) e = NDArr (Vector e)

deriving instance (Eq e, Storable e) => Eq (NDArr dim e)
deriving instance (Show e, Storable e) => Show (NDArr dim e)

(@) :: forall n m k . (KnownNat n, KnownNat m, KnownNat k)
    => NDArr [m, k] Double -> NDArr [k, n] Double -> NDArr [m, n] Double
NDArr a @ NDArr b = unsafePerformIO $ do
  let (aptr, alen) = V.unsafeToForeignPtr0 a
      (bptr, blen) = V.unsafeToForeignPtr0 b
      m = fromInteger $ fromSNat (SNat @m)
      n = fromInteger $ fromSNat (SNat @n)
      k = fromInteger $ fromSNat (SNat @k)
  cptr <- mallocForeignPtrBytes (m * n)
  -- Fortran (and BLAS) uses column-major indexing,
  -- so we switch the inputs order.
  withCString "N" \mode ->
    withForeignPtr aptr \aptr ->
      withForeignPtr bptr \bptr ->
        withForeignPtr cptr \cptr ->
          with (fromInteger $ fromSNat (SNat @m)) \m ->
            with (fromInteger $ fromSNat (SNat @n)) \n ->
              with (fromInteger $ fromSNat (SNat @k)) \k ->
                with 1 \alpha ->
                  with 0 \beta ->
                    gemm mode mode n m k alpha
                      bptr n
                      aptr k
                      beta cptr n
  let carr = V.unsafeFromForeignPtr0 cptr (m * n)
  pure $ NDArr carr

