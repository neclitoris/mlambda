{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE ViewPatterns #-}
module Test.MLambda.NDArr (testNDArr) where

import MLambda.Index
import MLambda.NDArr
import MLambda.TypeLits

import Test.MLambda.Utils

import Control.Monad
import Data.Bool.Singletons
import Data.List.Singletons (type (++))
import Data.Maybe
import Data.Proxy
import Data.Singletons
import GHC.TypeLits.Singletons
import Prelude.Singletons ((%+))
import Test.Falsify.Generator qualified as Gen
import Test.Falsify.Predicate ((.$))
import Test.Falsify.Predicate qualified as Pred
import Test.Tasty
import Test.Tasty.Falsify

propAtDotFromIndex :: Property ()
propAtDotFromIndex = do
  dim <- gen $ genDim 0 5
  let p = withIx dim \(Proxy @dim) -> do
        Fn f <- gen $ Gen.fun genInt
        i <- gen (genIndex @dim)
        assert $ Pred.eq
              .$ ("at . fromIndex", (at . fromIndex) f i)
              .$ ("id", f i)
  fromMaybe (error "propAtDotFromIndex: impossible") p

propFromIndexDotAt :: Property ()
propFromIndexDotAt = do
  dim <- gen $ genDim 0 5
  let p = withIx dim \(Proxy @dim) -> do
        arr <- gen $ genNDArr @dim genInt
        assert $ Pred.eq
              .$ ("fromIndex . at", (fromIndex . at) arr)
              .$ ("id", arr)
  fromMaybe (error "propFromIndexDotAt: impossible") p

testFromIndex :: TestTree
testFromIndex = testGroup "fromIndex"
  [ testProperty "at . fromIndex = id" propAtDotFromIndex
  , testProperty "fromIndex . at = id" propFromIndexDotAt]

propStack :: Property ()
propStack = do
  dimsuff <- gen $ genDim 0 3
  dimpref <- gen $ genDim 0 2
  dimmid1 <- gen genSz
  dimmid2 <- gen genSz
  case ( toSing dimsuff
       , toSing dimpref
       , toSing dimmid1
       , toSing dimmid2) of
    (SomeSing (singToIndexI -> Just (IxI @p))
      , SomeSing (singToIndexI -> Just (IxI @s))
      , SomeSing sm1@(SNat @m1), SomeSing sm2@(SNat @m2)) ->
        case ( sing @1 %<=? sm1
             , sing @1 %<=? sm2
             , sm1 %+ sm2
             , sing @1 %<=? sm1 %+ sm2) of
          (STrue, STrue, SNat, STrue) ->
            case ( concatIndexI (IxI @p) (IxI @(m1 : s))
                 , concatIndexI (IxI @p) (IxI @(m2 : s))
                 , concatIndexI (IxI @p) (IxI @((m1 + m2) : s))) of
              (IxI, IxI, IxI) -> do
                arr1 <- gen $ genNDArr @(p ++ (m1 : s)) genInt
                arr2 <- gen $ genNDArr @(p ++ (m2 : s)) genInt
                let arr3 = stackWithWitness (SW (Proxy @'(p, m1, m2, s))) arr1 arr2
                s <- gen $ genIndex @s
                p <- gen $ genIndex @p
                m <- gen $ genIndex @'[m1 + m2]
                let i1 = p `concatIndex` ((toEnum (fromEnum m) :: Index '[m1]) :. s)
                    i2 = p `concatIndex`
                      ((toEnum (fromEnum m - enumSize (Index [m1])) :: Index '[m2]) :. s)
                    i3 = p `concatIndex` (m :. s)
                if fromEnum m <= fromEnum (maxBound @(Index '[m1]))
                   then when (arr1 `at` i1 /= arr3 `at` i3) $ testFailed "mid index in lhs"
                   else when (arr2 `at` i2 /= arr3 `at` i3) $ testFailed "mid index in rhs"
          _ -> error "propStack: impossible"
    _ -> error "propStack: impossible"

testStack :: TestTree
testStack = testProperty "stack" propStack

testNDArr :: TestTree
testNDArr = testGroup "NDArr" [testFromIndex, testStack]
