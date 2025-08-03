{-# LANGUAGE RequiredTypeArguments #-}

module MLambda.Massiv (Mt, fromVector, mprod) where

import MLambda.TypeLits (KnownNat, natVal)

import Control.DeepSeq (NFData)
import Data.Massiv.Array qualified as Massiv
import Data.Massiv.Array.Manifest.Vector qualified as Massiv
import Data.Vector.Storable qualified as Vector

newtype Mt r e m n = MkMt (Massiv.Matrix r e)

deriving newtype instance NFData (Massiv.Matrix r e) => NFData (Mt r e m n)

size :: forall m n -> (KnownNat m, KnownNat n) => Massiv.Sz2
size m n = Massiv.Sz (natVal m Massiv.:. natVal n)

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
