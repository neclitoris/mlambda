{-# LANGUAGE TypeAbstractions #-}
module Test.MLambda.Matrix (testMatrix) where

import MLambda.Matrix
import MLambda.NDArr as NDArr
import MLambda.TypeLits

import Test.MLambda.Utils

import Control.Monad
import Data.Bool.Singletons
import Data.Singletons
import GHC.TypeLits.Singletons
import Test.Falsify.Predicate ((.$))
import Test.Falsify.Predicate qualified as Pred
import Test.Tasty
import Test.Tasty.Falsify

propId :: Property ()
propId = do
  (m, n) <- gen $ (,) <$> genSz <*> genSz
  (SomeSing (SNat @m)) <- pure $ toSing m
  (SomeSing (SNat @n)) <- pure $ toSing n
  STrue <- pure (sing @1 %<=? sing @m)
  STrue <- pure (sing @1 %<=? sing @n)
  a <- gen $ genNDArr @'[n, m] genDouble
  let a' = rep $ act a
  assert $ Pred.eq .$ ("rep . act", a') .$ ("id", a)

propCompose :: Property ()
propCompose = do
  (m, n) <- gen $ (,) <$> genSz <*> genSz
  (SomeSing (SNat @m)) <- pure $ toSing m
  (SomeSing (SNat @n)) <- pure $ toSing n
  (SomeSing (SNat @k)) <- pure $ toSing n
  STrue <- pure (sing @1 %<=? sing @m)
  STrue <- pure (sing @1 %<=? sing @n)
  STrue <- pure (sing @1 %<=? sing @k)
  a <- gen $ genNDArr @'[n, m] genDouble
  b <- gen $ genNDArr @'[m, k] genDouble
  let c = a `cross` b
      c' = rep (act a . act b)
      eps = 10**(-7)
      diff = NDArr.zipWith (\x y -> abs (x - y)) c c'
  forM_ [minBound..maxBound] $ \i ->
    when (diff `at` i > eps) $ testFailed "matrices unequal"


testMatmul :: TestTree
testMatmul = testGroup "matmul"
  [ testProperty "rep . act = id" propId
  , testProperty "rep (act a . act b) = a @ b" propCompose
  ]

testMatrix :: TestTree
testMatrix = testGroup "Matrix" [testMatmul]
