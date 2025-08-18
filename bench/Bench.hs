{-# LANGUAGE RequiredTypeArguments #-}

import MLambda.Matrix
import MLambda.NDArr
import MLambda.TypeLits (KnownNat, natVal)

import Data.Random.Normal (normalIO)
import Data.Vector.Storable qualified as Storable
import GHC.TypeLits (type (<=))
import System.Random (mkStdGen, setStdGen)
import Test.Tasty.Bench (bench, bgroup, defaultMain, env, nf, nfIO)

type M = 1000
type K = 1000
type N = 1000

setup :: IO (a -> b -> (a, b))
setup = (,) <$ setStdGen (mkStdGen 0)

mkNd :: forall m n -> (KnownNat m, KnownNat n, 1 <= m, 1 <= n) => IO (NDArr [m, n] Double)
mkNd m n = fromIndexM @[m, n] (const normalIO)

mkVec :: forall m n -> (KnownNat m, KnownNat n)
      => IO (Storable.Vector Double)
mkVec m n = Storable.replicateM (natVal n * natVal m) normalIO

main :: IO ()
main = defaultMain
  [ bgroup "random init"
    [ bench "NDArr" $ nfIO $ mkNd M N
    , bench "Storable.Vector" $ nfIO $ mkVec M N
    ]
  , env (setup <*> mkNd M K <*> mkNd K N) \input ->
    bgroup "matmul"
    [ bench "Massiv" $ nf (uncurry crossMassiv) input
    , bench "OpenBLAS" $ nf (uncurry cross) input
    , bench "Naive" $ nf (uncurry crossNaive) input
    ]
  ]
