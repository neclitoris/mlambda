{-# LANGUAGE QuasiQuotes #-}

import Test.MLambda.NDArr
import Test.Tasty
import Test.Tasty.HUnit

import MLambda.Matrix
import MLambda.NDArr

main :: IO ()
main = defaultMain tests

a :: NDArr [2, 3] Double
a = [mat| 1 2 3
          4 5 6 |]

b :: NDArr [3, 1] Double
b = [mat| 1
          -1
          0 |]

c :: NDArr [2, 1] Double
c = [mat| -1
          -1 |]

tests :: TestTree
tests = testGroup "Tests"
  [ testCase "Mul" $
      a `cross` b @?= c
  , testNDArr
  ]
