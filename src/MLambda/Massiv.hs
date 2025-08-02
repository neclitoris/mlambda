{-# LANGUAGE RequiredTypeArguments #-}

module MLambda.Massiv (Mt, makeMt, mprod, value) where

import Control.DeepSeq (NFData)
import Data.Massiv.Array qualified as Massiv
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownNat, natVal)

newtype Mt r e m n = MkMt (Massiv.Matrix r e)

deriving newtype instance NFData (Massiv.Matrix r e) => NFData (Mt r e m n)

value :: forall k -> (KnownNat k, Num a) => a
value k = fromInteger $ natVal (Proxy @k)

makeMt ::
    forall m n e r. (Massiv.Load r Massiv.Ix2 e, KnownNat m, KnownNat n) =>
    Massiv.Comp -> (Massiv.Ix2 -> e) -> Mt r e m n
makeMt c = MkMt . Massiv.makeArray c (Massiv.Sz (value m Massiv.:. value n))

mprod ::
    (Massiv.Numeric r e, Massiv.Manifest r e) =>
    Mt r e m k -> Mt r e k n -> Mt r e m n
mprod (MkMt m) (MkMt n) = MkMt (m Massiv.!><! n)
