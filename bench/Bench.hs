{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RequiredTypeArguments #-}

import MLambda.Matrix
import MLambda.NDArr
import MLambda.TypeLits (KnownNat, natVal)

import Data.Massiv.Array (Array, Comp (..), Ix2, pattern Sz2)
import Data.Massiv.Array.Manifest (S)
import Data.Massiv.Array.Mutable (freeze, makeMArrayS)
import Data.Primitive.PrimVar
import Data.Random.Normal (normalIO)
import Data.Vector.Storable qualified as Storable
import Foreign.Storable
import GHC.TypeLits (type (*), type (<=))
import System.Random (mkStdGen, setStdGen)
import Test.Tasty (localOption)
import Test.Tasty.Bench
       (TimeMode (..), bench, bgroup, defaultMain, env, nf, nfIO)

type M = 100
type K = 100
type N = 100

setup :: IO (a -> b -> (a, b))
setup = (,) <$ setStdGen (mkStdGen 0)

mkNd :: forall m n -> (KnownNat m, KnownNat n, 1 <= m, 1 <= n, Storable a)
      => IO a -> IO (NDArr [m, n] a)
mkNd m n gen = fromIndexM @'[m, n] $ const gen

mkVec :: forall m n -> (KnownNat m, KnownNat n, Storable a)
      => IO a -> IO (Storable.Vector a)
mkVec m n gen = Storable.replicateM (natVal n * natVal m) $ gen

mkMassiv :: forall m n -> (KnownNat m, KnownNat n, Storable a) => IO a -> IO (Array S Ix2 a)
mkMassiv m n gen = freeze Seq =<< makeMArrayS (Sz2 (natVal n) (natVal m)) (const gen)

main :: IO ()
main = do
  var <- newPrimVar 0
  defaultMain $ localOption WallTime <$>
    [ bgroup "primvar iota init"
      [ bench "NDArr" $ nfIO $ mkNd M N (fetchAddInt var 1)
      , bench "Storable.Vector" $ nfIO $ mkVec M N (fetchAddInt var 1)
      , bench "massiv" $ nfIO $ mkMassiv M N (fetchAddInt var 1)
      ]
    , bgroup "random init"
      [ bench "NDArr" $ nfIO $ mkNd @Double M N normalIO
      , bench "Storable.Vector" $ nfIO $ mkVec @Double M N normalIO
      , bench "massiv" $ nfIO $ mkMassiv @Double M N normalIO
      ]
    , env (setup <*> mkNd @Double M K normalIO <*> mkNd @Double K N normalIO) \input ->
      bgroup "matmul"
      [ bench "Massiv" $ nf (uncurry crossMassiv) input
      , bench "OpenBLAS" $ nf (uncurry cross) input
      , bench "Naive" $ nf (uncurry crossNaive) input
      ]
    ]
