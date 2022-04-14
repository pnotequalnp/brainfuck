-- |
-- Module      : Language.Brainfuck.Interpreter
-- Description : Brainfuck Interpreter
-- Copyright   : Kevin Mullins 2022
-- License     : ISC
-- Maintainer  : kevin@pnotequalnp.com
-- Stability   : unstable
--
-- = Brainfuck Interpreter
-- This is a basic Brainfuck language interpreter using a mutable vector as a buffer.
module Language.Brainfuck.Interpreter (
  -- * Interpreter
  interpret,
  executeStatement,

  -- * Pointers
  Pointer,
  incPtr,
  decPtr,
  ptr,
  withPtr,
) where

import Control.Monad.Loops (whileM_)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Vector.Unboxed.Mutable (IOVector)
import Data.Vector.Unboxed.Mutable qualified as V
import Data.Word (Word8)
import Language.Brainfuck.Interpreter.Internal (getByte, putByte)
import Language.Brainfuck.Syntax (Program, Statement (..))

-- | Implicit pointer parameter.
type Pointer = (?pointer :: IORef Int)

-- | Increment the pointer by 1.
incPtr :: Pointer => IO ()
incPtr = modifyIORef' ?pointer (+ 1)

-- | Decrement the pointer by 1.
decPtr :: Pointer => IO ()
decPtr = modifyIORef' ?pointer (- 1)

-- | Get the current value of the pointer.
ptr :: Pointer => IO Int
ptr = readIORef ?pointer

-- | Inject a pointer into an expression.
withPtr :: IORef Int -> (Pointer => a) -> a
withPtr pointer x = x
  where
    ?pointer = pointer

-- | Interpret a Brainfuck program by its AST.
interpret ::
  -- | Memory size in bytes
  Word ->
  -- | Program to interpret
  Program ->
  IO ()
interpret memSize program = do
  memory <- V.replicate (fromIntegral memSize) 0
  pointer <- newIORef 0
  withPtr pointer $ traverse_ (executeStatement memory) program

-- | Execute a single Brainfuck `Statement`.
executeStatement ::
  Pointer =>
  -- | Memory
  IOVector Word8 ->
  -- | Statement to execute
  Statement ->
  IO ()
executeStatement memory = \case
  ShiftL -> decPtr
  ShiftR -> incPtr
  Inc -> ptr >>= V.modify memory (+ 1)
  Dec -> ptr >>= V.modify memory (- 1)
  Output -> ptr >>= V.read memory >>= putByte
  Input -> do
    x <- getByte
    ptr >>= flip (V.write memory) x
  Loop statements ->
    whileM_ (ptr >>= V.read memory <&> (/= 0)) $
      traverse_ (executeStatement memory) statements
