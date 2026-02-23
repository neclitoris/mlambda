{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RequiredTypeArguments #-}

import MLambda.Matrix
import MLambda.NDArr
import MLambda.TypeLits (KnownNat, natVal)

import Data.Massiv.Array (Array, Comp (..), Ix2, pattern Sz2)
import Data.Massiv.Array.Manifest (S)
import Data.Massiv.Array.Mutable (freeze, makeMArrayS)
import Data.Random.Normal (normalIO)
import Data.Vector.Storable qualified as Storable
import GHC.TypeLits (type (<=))
import System.Random (mkStdGen, setStdGen)
import Test.Tasty.Bench (bench, bgroup, defaultMain, env, nf, nfIO)

type M = 100
type K = 100
type N = 100

setup :: IO (a -> b -> (a, b))
setup = (,) <$ setStdGen (mkStdGen 0)

mkNd :: forall m n -> (KnownNat m, KnownNat n, 1 <= m, 1 <= n) => IO (NDArr [m, n] Double)
mkNd m n = fromIndexM @[m, n] (const normalIO)

mkVec :: forall m n -> (KnownNat m, KnownNat n)
      => IO (Storable.Vector Double)
mkVec m n = Storable.replicateM (natVal n * natVal m) normalIO

mkMassiv :: forall m n -> (KnownNat m, KnownNat n) => IO (Array S Ix2 Double)
mkMassiv m n = freeze Seq =<< makeMArrayS (Sz2 (natVal n) (natVal m)) (const normalIO)

main :: IO ()
main = defaultMain
  [ bgroup "random init"
    [ bench "NDArr" $ nfIO $ mkNd M N
    , bench "Storable.Vector" $ nfIO $ mkVec M N
    , bench "massiv" $ nfIO $ mkMassiv M N
    ]
  , env (setup <*> mkNd M K <*> mkNd K N) \input ->
    bgroup "matmul"
    [ bench "Massiv" $ nf (uncurry crossMassiv) input
    , bench "OpenBLAS" $ nf (uncurry cross) input
    , bench "Naive" $ nf (uncurry crossNaive) input
    ]
  ]
