{- |
 Module      : Brainfuck.Interpreter.IO
 Description : Brainfuck Interpreter IO Utilities
 Copyright   : Kevin Mullins 2022
 License     : ISC
 Maintainer  : kevin@pnotequalnp.com
-}
module Brainfuck.Interpreter.IO where

import Foreign (Storable (..), alloca, with)
import System.IO (hGetBuf, hPutBuf, stdin, stdout)

-- | Input function for `IO`, reading from `stdin`, giving @0@ for EOF
stdinInputZero :: forall byte. (Num byte, Storable byte) => byte -> IO byte
stdinInputZero _ = alloca \buf -> do
  let size = sizeOf @byte undefined
  n <- hGetBuf stdin buf size
  if n < size
    then pure 0
    else peek buf

-- | Output function for `IO`, writing to `stdin`
stdoutOutput :: Storable byte => byte -> IO ()
stdoutOutput x = with x \buf -> do
  hPutBuf stdout buf (sizeOf x)
