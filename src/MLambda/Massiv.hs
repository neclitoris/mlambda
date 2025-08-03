{-# LANGUAGE RequiredTypeArguments #-}

module MLambda.Massiv (Mt, fromVector, mprod, value) where

import Control.DeepSeq (NFData)
import Data.Massiv.Array qualified as Massiv
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownNat, natVal)
import qualified Data.Vector.Storable as Vector
import qualified Data.Massiv.Array.Manifest.Vector as Massiv

newtype Mt r e m n = MkMt (Massiv.Matrix r e)

deriving newtype instance NFData (Massiv.Matrix r e) => NFData (Mt r e m n)

value :: forall k -> (KnownNat k, Num a) => a
value k = fromInteger $ natVal (Proxy @k)

size :: forall m n -> (KnownNat m, KnownNat n) => Massiv.Sz2
size m n = Massiv.Sz (value m Massiv.:. value n)

fromVector ::
    forall k n e r m.
    ( Massiv.MonadThrow m, Vector.Storable e, Massiv.Manifest r e
    , Massiv.Load r Massiv.Ix2 e, KnownNat k, KnownNat n
    ) => Massiv.Comp -> Vector.Vector e -> m (Mt r e k n)
fromVector c = fmap MkMt . Massiv.fromVectorM c (size k n)

mprod ::
    (Massiv.Numeric r e, Massiv.Manifest r e) =>
    Mt r e m k -> Mt r e k n -> Mt r e m n
mprod (MkMt m) (MkMt n) = MkMt (m Massiv.!><! n)
