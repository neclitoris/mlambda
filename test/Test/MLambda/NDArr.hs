module Test.MLambda.NDArr (testNDArr) where

import MLambda.Index
import MLambda.NDArr

import Control.Monad
import Data.Proxy
import Data.Singletons
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

instance Enum (Index dim) => Gen.Function (Index dim) where
  function gb = Gen.functionMap fromEnum toEnum <$> Gen.function gb

propFromIndex :: Property ()
propFromIndex = do
  dim <- gen genDim
  let Just p = withIx dim \(Proxy @dim) -> do
                 Fn f <- gen $ Gen.fun genInt
                 let is = [minBound..maxBound] :: [Index dim]
                     ndarr = fromIndex f
                     values = map f is
                     ndarrValues = map (ndarr `at`) is
                     pred = Pred.relatedBy
                       ("==", (or .) . zipWith (==))
                 assert $ pred .$ ("fromIndex f `at` i", ndarrValues)
                               .$ ("f i", values)
  p

testFromIndex :: TestTree
testFromIndex = testProperty "fromIndex" propFromIndex

testNDArr :: TestTree
testNDArr = testGroup "NDArr" [testFromIndex]
