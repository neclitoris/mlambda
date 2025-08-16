{-# LANGUAGE TypeAbstractions #-}
module Test.MLambda.NDArr (testNDArr) where

import MLambda.Index
import MLambda.NDArr

import Data.Maybe
import Data.Proxy
import Numeric.Natural
import Test.Falsify.Generator qualified as Gen
import Test.Falsify.Predicate ((.$))
import Test.Falsify.Predicate qualified as Pred
import Test.Falsify.Range qualified as Range
import Test.Tasty
import Test.Tasty.Falsify

genSz :: Gen Natural
genSz = fromIntegral <$> Gen.int (Range.between (1, 5))

genDim :: Gen [Natural]
genDim = Gen.list (Range.between (1, 5)) genSz

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
  Fn f <- Gen.fun genInt
  pure $ fromIndex f

instance Enum (Index dim) => Gen.Function (Index dim) where
  function gb = Gen.functionMap fromEnum toEnum <$> Gen.function gb

propAtDotFromIndex :: Property ()
propAtDotFromIndex = do
  dim <- gen genDim
  let p = withIx dim \(Proxy @dim) -> do
        Fn f <- gen $ Gen.fun genInt
        i <- gen (genIndex @dim)
        assert $ Pred.eq
              .$ ("at . fromIndex", (at . fromIndex) f i)
              .$ ("id", f i)
  fromMaybe (error "propAtDotFromIndex: impossible") p

propFromIndexDotAt :: Property ()
propFromIndexDotAt = do
  dim <- gen genDim
  let p = withIx dim \(Proxy @dim) ->
        case inst @dim of
          _ -> do
            arr <- gen (genNDArr @dim)
            assert $ Pred.eq
                  .$ ("fromIndex . at", (fromIndex . at) arr)
                  .$ ("id", arr)
  fromMaybe (error "propFromIndexDotAt: impossible") p

testFromIndex :: TestTree
testFromIndex = testGroup "fromIndex"
  [ testProperty "at . fromIndex = id" propAtDotFromIndex
  , testProperty "fromIndex . at = id" propFromIndexDotAt]

testNDArr :: TestTree
testNDArr = testGroup "NDArr" [testFromIndex]
