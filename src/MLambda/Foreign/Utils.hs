-- |
-- Module      : MLambda.Matrix
-- Description : Basic ndarray type.
-- Copyright   : (c) neclitoris, 2025
-- License     : BSD-3-Clause
-- Maintainer  : nas140301@gmail.com
-- Stability   : experimental
-- Portability : portable
--
module MLambda.Foreign.Utils
  ( module Numeric.Netlib.Utility
  , asPtr
  , asFPtr
  ) where

import Control.Monad.Cont
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Numeric.Netlib.Utility

asPtr :: Storable a => a -> ContT r IO (Ptr a)
asPtr = ContT . with

asFPtr :: ForeignPtr a -> ContT r IO (Ptr a)
asFPtr = ContT . withForeignPtr
