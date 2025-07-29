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
