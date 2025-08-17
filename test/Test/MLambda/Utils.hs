module Test.MLambda.Utils where

import MLambda.Index
import MLambda.NDArr
import MLambda.TypeLits

import Data.Proxy
import Test.Falsify.Generator (Gen)
import Test.Falsify.Generator qualified as Gen
import Test.Falsify.Range (Range)
import Test.Falsify.Range qualified as Range

genSz :: Gen Natural
genSz = fromIntegral <$> Gen.int (Range.between (1, 5))

genDim :: Word -> Word -> Gen [Natural]
genDim a b = Gen.list (Range.between (a, b)) genSz

genInt :: Gen Int
genInt = Gen.inRange $ Range.between (-1000, 1000)

genIndex :: forall dim . Ix dim => Gen (Index dim)
genIndex = case inst @dim of
  EI -> pure E
  Proxy @h :.= (_ :: IndexI t) -> do
    h <- genInt
    t <- genIndex @t
    pure ((toEnum h :: Index '[h]) :. t)

genNDArr :: forall dim . Ix dim => Gen (NDArr dim Int)
genNDArr = do
  Gen.Fn f <- Gen.fun genInt
  pure $ fromIndex f

instance Enum (Index dim) => Gen.Function (Index dim) where
  function gb = Gen.functionMap fromEnum toEnum <$> Gen.function gb

