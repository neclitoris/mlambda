{-# LANGUAGE RequiredTypeArguments #-}

import Test.Tasty.Bench (bench, defaultMain, env, nf)

import Data.Massiv.Array (Comp (Par), Ix2 (..), P)
import Data.Vector.Storable (fromList)
import GHC.TypeLits (KnownNat)

import MLambda.Massiv (Mt, makeMt, mprod, value)
import MLambda.Matrix (NDArr (MkNDArr), cross)

type M = 10
type K = 10
type N = 10

mkMt :: forall m n -> (KnownNat m, KnownNat n) => Mt P Double m n
mkMt _ _ = makeMt Par \(i :. j) -> fromIntegral (i * j)

mkNd :: forall m n -> (KnownNat m, KnownNat n) => NDArr [m, n] Double
mkNd m n = MkNDArr $ fromList
    [ fromInteger (i * j) | i <- [0..value m - 1], j <- [0..value n - 1] ]

main :: IO ()
main = defaultMain
  [ env (return (mkMt M K, mkMt K N)) $ bench "Massiv matmul" . nf (uncurry mprod)
  , env (return (mkNd M K, mkNd K N)) $ bench "OpenBLAS matmul" . nf (uncurry cross)
  ]
