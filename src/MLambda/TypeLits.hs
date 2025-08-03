{-# LANGUAGE RequiredTypeArguments #-}

module MLambda.TypeLits (KnownNat, natVal) where

import GHC.TypeLits (KnownNat, fromSNat, natSing)

natVal :: forall n -> (KnownNat n, Num a) => a
natVal n = fromInteger $ fromSNat (natSing @n)
