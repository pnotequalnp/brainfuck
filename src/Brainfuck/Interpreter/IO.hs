{- |
 Module      : Brainfuck.Interpreter.IO
 Description : Brainfuck Interpreter IO Utilities
 Copyright   : Kevin Mullins 2022
 License     : ISC
 Maintainer  : kevin@pnotequalnp.com
-}
module Brainfuck.Interpreter.IO where

import Brainfuck.Configuration (EofBehavior (..))
import Foreign (Storable (..), alloca, with)
import System.IO (Handle, hGetBuf, hPutBuf)

-- | Input function for `IO`, reading from `stdin`, giving @0@ for EOF
handleInput :: forall byte. (Num byte, Storable byte) => Handle -> EofBehavior -> byte -> IO byte
handleInput h onEof x = alloca \buf -> do
  let size = sizeOf @byte undefined
  n <- hGetBuf h buf size
  if n < size
    then pure case onEof of
      Zero -> 0
      Unchanged -> x
      NegativeOne -> -1
    else peek buf

-- | Output function for `IO`, writing to `stdout`
handleOutput :: Storable byte => Handle -> byte -> IO ()
handleOutput h x = with x \buf -> do
  hPutBuf h buf (sizeOf x)
