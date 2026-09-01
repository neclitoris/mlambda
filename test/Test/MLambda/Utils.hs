{-# LANGUAGE TypeAbstractions #-}
module Test.MLambda.Utils where

import MLambda.Index
import MLambda.NDArr
import MLambda.TypeLits

import Data.Falsify.ConcreteFun as D
import Data.Falsify.ProperFraction as D
import Data.Proxy
import Foreign.Storable
import Test.Falsify
import Test.Falsify.Generator (Gen)
import Test.Falsify.Generator qualified as Gen
import Test.Falsify.Range qualified as Range

genSz :: Gen Natural
genSz = fromIntegral <$> Gen.int (Range.inclusive (1, 5))

genDim :: Word -> Word -> Gen [Natural]
genDim a b = Gen.list (Range.inclusive (a, b)) genSz

genInt :: Gen Int
genInt = Gen.inRange $ Range.inclusive (-1000, 1000)

genDouble :: Gen Double
genDouble = Gen.inRange $ Range.fromProperFraction 64
  \(D.ProperFraction d) -> 1 + 4 * d

genIndex :: forall dim . Ix dim => Gen (Index dim)
genIndex = case inst @dim of
  EI -> pure E
  Proxy @h :.= (_ :: IndexI t) -> do
    h <- genInt
    t <- genIndex @t
    pure ((toEnum h :: Index '[h]) :. t)

genNDArr :: forall dim e . (Storable e, Ix dim) => Gen e -> Gen (NDArr dim e)
genNDArr g = do
  Fn f <- Gen.fun g
  pure $ fromIndex f

instance Enum (Index dim) => Gen.Function (Index dim) where
  function gb = D.map fromEnum toEnum <$> Gen.function gb

