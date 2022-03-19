{-# LANGUAGE CApiFFI #-}

module Language.Brainfuck.Interpreter.Internal where

import Data.Word (Word8)
import Foreign.C (CInt (..), throwErrnoIfMinus1Retry, throwErrnoIfMinus1Retry_)

foreign import capi unsafe "stdio.h putchar"
  c_putchar :: CInt -> IO CInt

putByte :: Word8 -> IO ()
putByte x = throwErrnoIfMinus1Retry_ "putByte: failed to write byte" (c_putchar (fromIntegral x))

foreign import capi unsafe "stdio.h getchar"
  c_getchar :: IO CInt

getByte :: IO Word8
getByte = fromIntegral <$> throwErrnoIfMinus1Retry "getByte: failed to get byte" c_getchar
