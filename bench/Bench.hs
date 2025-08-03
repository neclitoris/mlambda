{-# LANGUAGE RequiredTypeArguments #-}

import MLambda.Matrix
import MLambda.TypeLits (KnownNat, natVal)

import Data.Random.Normal (normalIO)
import Data.Vector.Storable (replicateM)
import System.Random (mkStdGen, setStdGen)
import Test.Tasty.Bench (bench, bgroup, defaultMain, env, nf, nfIO)

type M = 1000
type K = 1000
type N = 1000

setup :: IO (a -> b -> (a, b))
setup = (,) <$ setStdGen (mkStdGen 0)

mkNd :: forall m n -> (KnownNat m, KnownNat n) => IO (NDArr [m, n] Double)
mkNd m n = MkNDArr <$> replicateM (natVal m * natVal n) normalIO

main :: IO ()
main = defaultMain
  [ bench "random init" $ nfIO (mkNd M N)
  , env (setup <*> mkNd M K <*> mkNd K N) \input ->
    bgroup "matmul"
    [ bench "Massiv" $ nf (uncurry crossMassiv) input
    , bench "OpenBLAS" $ nf (uncurry cross) input
    ]
  ]
