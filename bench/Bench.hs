{-# LANGUAGE RequiredTypeArguments #-}

import Data.Massiv.Array (Comp (Par), P)
import Data.Random.Normal (normalIO)
import Data.Vector.Storable (replicateM, Vector)
import GHC.TypeLits (KnownNat)
import System.Random (mkStdGen, setStdGen)
import Test.Tasty.Bench (bench, defaultMain, env, nf, bgroup, nfIO)

import MLambda.Massiv (Mt, fromVector, mprod, value)
import MLambda.Matrix (NDArr (MkNDArr), cross)

type M = 1000
type K = 1000
type N = 1000

setup :: IO (a -> b -> (a, b))
setup = (,) <$ setStdGen (mkStdGen 0)

mkVec :: forall m n -> (KnownNat m, KnownNat n) => IO (Vector Double)
mkVec m n = replicateM (value m * value n) normalIO

mkMt :: forall m n -> (KnownNat m, KnownNat n) => IO (Mt P Double m n)
mkMt m n = mkVec m n >>= fromVector Par

mkNd :: forall m n -> (KnownNat m, KnownNat n) => IO (NDArr [m, n] Double)
mkNd m n = MkNDArr <$> mkVec m n

main :: IO ()
main = defaultMain
  [ bgroup "random init"
    [ bench "Massiv" $ nfIO (mkMt M N)
    , bench "Vector" $ nfIO (mkNd M N)
    ]
  , bgroup "matmul"
    [ env (setup <*> mkMt M K <*> mkMt K N) $
      bench "Massiv" . nf (uncurry mprod)
    , env (setup <*> mkNd M K <*> mkNd K N) $
      bench "OpenBLAS" . nf (uncurry cross)
    ]
  ]
