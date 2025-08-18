{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : MLambda.Matrix
-- Description : Matrix operations on 'NDArr'ays.
-- Copyright   : (c) neclitoris, TurtlePU, 2025
-- License     : BSD-3-Clause
-- Maintainer  : nas140301@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module contains matrix API of basic 'NDArr' type.
module MLambda.Matrix
  -- * Matrix multiplication
  ( cross
  , crossMassiv
  , crossNaive
  -- * Matrix creation
  , mat
  , eye
  , rep
  , act
  -- * Shape manipulation
  , transpose
  ) where

import MLambda.Foreign.Utils (asFPtr, asPtr, char)
import MLambda.Index
import MLambda.Linear
import MLambda.NDArr hiding (concat, foldr, map, zipWith)
import MLambda.NDArr qualified as NDArr
import MLambda.TypeLits

import Control.Applicative
import Control.Exception (assert)
import Control.Monad
import Control.Monad.Cont
import Control.Monad.IO.Class
import Control.Monad.ST (runST)
import Data.Char
import Data.Either (fromLeft, fromRight, isLeft)
import Data.List (findIndices)
import Data.Massiv.Array qualified as Massiv
import Data.Massiv.Array.Manifest.Vector qualified as Massiv
import Data.Maybe
import Data.Vector.Storable qualified as Storable
import Data.Vector.Storable.Mutable qualified as Mutable
import Foreign.ForeignPtr
import Foreign.Storable
import GHC.IO hiding (liftIO)
import GHC.Ptr
import Language.Haskell.Meta.Parse qualified as Haskell
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote qualified as Quote
import Numeric.BLAS.FFI.Generic
import Numeric.Netlib.Class qualified as BLAS
import Text.ParserCombinators.ReadP qualified as P

massivSize :: forall m n -> (KnownNat m, KnownNat n) => Massiv.Sz2
massivSize m n = natVal m `Massiv.Sz2` natVal n

fromMassiv ::
  forall m n e. (KnownNat m, KnownNat n, Storable e) =>
  Massiv.Array Massiv.S Massiv.Ix2 e -> NDArr [m, n] e
fromMassiv arr =
  unsafeMkNDArr $ Massiv.toVector $ assert (Massiv.size arr == massivSize m n) arr

toMassiv ::
  forall m n e. (KnownNat m, KnownNat n, Storable e) =>
  NDArr [m, n] e -> Massiv.Array Massiv.S Massiv.Ix2 e
toMassiv = Massiv.fromVector' Massiv.Par (massivSize m n) . runNDArr

-- | Matrix product reused from massiv.
crossMassiv ::
  (KnownNat m, KnownNat k, KnownNat n, Num e, Storable e) =>
  NDArr [m, k] e -> NDArr [k, n] e -> NDArr [m, n] e
crossMassiv a b = fromMassiv $ toMassiv a Massiv.!><! toMassiv b

-- | Your usual matrix product. Calls into BLAS's @gemm@ operation.
cross :: forall m k n e . (KnownNat n, KnownNat m, KnownNat k, BLAS.Floating e)
    => NDArr [m, k] e -> NDArr [k, n] e -> NDArr [m, n] e
(runNDArr -> a) `cross` (runNDArr -> b) = unsafePerformIO $ evalContT do
  let (afptr, _alen) = Storable.unsafeToForeignPtr0 a
      (bfptr, _blen) = Storable.unsafeToForeignPtr0 b
      len = natVal m * natVal n
  cfptr <- liftIO $ mallocForeignPtrArray len
  -- Fortran (and BLAS) uses column-major indexing,
  -- so we switch the inputs order.
  mode <- char 'N'
  aptr <- asFPtr afptr
  bptr <- asFPtr bfptr
  cptr <- asFPtr cfptr
  mptr <- asPtr $ natVal m
  nptr <- asPtr $ natVal n
  kptr <- asPtr $ natVal k
  alpha <- asPtr 1
  beta <- asPtr 0
  liftIO $ gemm mode mode nptr mptr kptr alpha
      bptr nptr
      aptr kptr
      beta cptr nptr
  let carr = Storable.unsafeFromForeignPtr0 cfptr len
  pure $ unsafeMkNDArr carr

crossGeneric :: forall m k n e1 e2 e3 .
                ( KnownNat n, KnownNat m, KnownNat k
                , 1 <= n, 1 <= m, 1 <= k
                , Storable e1, Storable e2, Storable e3)
             => (e1 -> e2 -> e3) -> (e3 -> e3 -> e3)
             -> NDArr '[m, k] e1 -> NDArr '[k, n] e2 -> NDArr '[m, n] e3
crossGeneric mul plus a b = runST do
  mvec <- Mutable.new (natVal m * natVal n)
  forM_ [minBound..maxBound :: Index '[m]] \i ->
    forM_ [minBound..maxBound :: Index '[k]] \k ->
      forM_ [minBound..maxBound :: Index '[n]] \j ->
        Mutable.modify mvec
          (plus (mul (a `at` (i :. k)) (b `at` (k :. j))))
          (fromEnum (i :. j))
  unsafeMkNDArr @'[m, n] <$> Storable.unsafeFreeze mvec

-- | Naive implementation of matrix multiplication.
crossNaive :: ( KnownNat n, KnownNat m, KnownNat k
              , 1 <= n, 1 <= m, 1 <= k
              , Storable e, Num e)
           => NDArr '[m, k] e -> NDArr '[k, n] e -> NDArr '[m, n] e
crossNaive = crossGeneric (*) (+)

infixl 7 `cross`
infixl 7 `crossMassiv`

-- | @mat@ is a quasi-quote used to safely define matrices.
-- Usage:
--
-- > [mat| 1 2 3
-- >       4 5 6 |]
--
-- This quote will fail if type of the expression it appears in
-- is not a 2x3 matrix.
--
-- Haskell expressions can be spliced in, too. For example,
-- it can be used to define a 2-dimensional rotation matrix like this:
--
-- > rot phi = [mat| $(cos phi) $(-sin phi)
-- >                 $(sin phi) $(cos phi)  |]
--
-- If the expression is just a single variable, parentheses can be omitted:
--
-- > [mat | $x $y
-- >        0  $z |]
--
-- Whitespace inside the quote is not significant.
mat :: Quote.QuasiQuoter
mat = Quote.QuasiQuoter
  { quoteExp = matE
  , quotePat = error "Not implemented"
  , quoteType = error "Not implemented"
  , quoteDec = error "Not implemented"
  }

matE :: String -> TH.Q TH.Exp
matE s = do
  let expr prev =
        TH.dyn <$> liftM2 (:) (P.satisfy isAlpha) (P.munch isAlphaNum)
          <|>
        do
          str <- P.manyTill P.get $ P.char ')'
          case Haskell.parseExp (prev ++ str ++ ")") of
            Right e -> pure $ pure e
            Left _  -> expr (prev ++ str ++ ")")
      item = Right <$> P.readS_to_P (reads @Double)
         <|> Left <$> (P.char '$' *> expr "")
      space = P.skipMany $ P.char ' '
      line = space *>P.endBy1 item space
      matrix = P.skipSpaces *> P.sepBy1 line (P.char '\n') <* P.skipSpaces <* P.eof
  l <- maybe (fail "Failed parse") (pure . fst) $ listToMaybe $ P.readP_to_S matrix s
  let m = length l
  n <- maybe (fail "Empty matrix specified") (pure . length) $ listToMaybe l
  when (n == 0) $ fail "Empty matrix specified"
  when (any ((/= n) . length) l) $ fail "Wrong column count"
  let indices = concat $ zipWith (<*>) (map ((:[]) . (+)) [0, n..]) $ map (findIndices isLeft) l
  let exps = map (fromLeft (fail "You encountered a bug in the `mat` quasiquoter!"))
        $ concatMap (filter isLeft) l
  let l' = map (fromRight 0) $ concat l
  let dat = Storable.fromList l'
      (fptr, offset, len) =
        Storable.unsafeToForeignPtr $ Storable.unsafeCast dat

  let bytes = TH.litE $ TH.bytesPrimL
            $ TH.mkBytes fptr (fromIntegral offset)
            $ fromIntegral len
      size = m * n
      tm = TH.litT $ TH.numTyLit $ toInteger m
      tn = TH.litT $ TH.numTyLit $ toInteger n
  let raw = [|
        unsafeDupablePerformIO do
          ptr <- newForeignPtr_ $ Ptr $bytes
          pure $ Storable.unsafeFromForeignPtr0 ptr size
        |]
  if null indices
    then [| unsafeMkNDArr @'[$tm, $tn] @Double $raw |]
    else [| runST do
              mvec <- Storable.thaw $raw
              $(TH.doE $ flip map (zip indices exps)
                \(i, e) -> TH.noBindS [| Mutable.write mvec i $e |])
              vec <- Storable.unsafeFreeze mvec
              pure $ unsafeMkNDArr @'[$tm, $tn] @Double vec
         |]

-- | Identity matrix.
eye :: (KnownNat n, 1 <= n, Storable e, Num e) => NDArr '[n, n] e
eye = fromIndex go where
  go (i :. j) | i == j    = 1
              | otherwise = 0

-- | A matrix that corresponds to a linear map of vector spaces.
rep :: forall m n e .
       ( KnownNat m, 1 <= m
       , KnownNat n, 1 <= n
       , Storable e, Num e)
    => LinearMap' Storable (NDArr '[m]) (NDArr '[n]) e -> NDArr '[n, m] e
rep f = transpose $ NDArr.concat $ fromIndex (f . (rows @'[m] eye `at`))

-- | A linear map of vector spaces that corresponds to a matrix.
act :: forall m n e .
       ( KnownNat m, 1 <= m
       , KnownNat n, 1 <= n
       , Storable e)
    => NDArr '[n, m] e -> LinearMap' Storable (NDArr '[m]) (NDArr '[n]) e
act a = reshape [n] . crossGeneric modMult add a . reshape [m, 1]

-- | Matrix transposition.
transpose :: (KnownNat m, 1 <= m, KnownNat n, 1 <= n, Storable e)
          => NDArr '[m, n] e -> NDArr '[n, m] e
transpose m = fromIndex \(i :. j) -> m `at` (j :. i)
