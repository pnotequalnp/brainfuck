{-# LANGUAGE StrictData #-}

{- |
 Module      : Brainfuck.Interpreter
 Description : Brainfuck Interpreter
 Copyright   : Kevin Mullins 2022
 License     : ISC
 Maintainer  : kevin@pnotequalnp.com
-}
module Brainfuck.Interpreter where

import Brainfuck.Syntax
import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad (..))
import Data.Foldable (sequenceA_, traverse_)
import Data.Primitive (modifyMutVar', newMutVar)
import Data.Primitive.MutVar (MutVar, readMutVar)
import Data.Vector.Unboxed.Mutable (MVector, Unbox, modify, modifyM, new, read, write)
import Prelude hiding (read)

-- | Interpret a brainfuck program
interpret ::
  (PrimMonad m, Num byte, Eq byte, Unbox byte) =>
  -- | Input function
  (byte -> m byte) ->
  -- | Output function
  (byte -> m ()) ->
  -- | Memory size in bytes
  Int ->
  -- | Initial pointer location
  Int ->
  -- | Brainfuck program
  [Brainfuck byte Int] ->
  m (MVector (PrimState m) byte, Int)
interpret input output memory ptr program = do
  buffer <- new (fromIntegral memory)
  pointer <- newMutVar ptr
  traverse_ (execute input output buffer pointer) program
  ptr' <- readMutVar pointer
  pure (buffer, ptr')

-- | Execute a single brainfuck instruction
execute ::
  (PrimMonad m, Num byte, Eq byte, Unbox byte) =>
  -- | Input function
  (byte -> m byte) ->
  -- | Output function
  (byte -> m ()) ->
  -- | Memory cells
  MVector (PrimState m) byte ->
  -- | Pointer
  MutVar (PrimState m) Int ->
  -- | Brainfuck instruction
  Brainfuck byte Int ->
  m ()
execute input output buffer pointer = cata \case
  AddF amount offset -> do
    ptr <- readMutVar pointer
    modify buffer (+ amount) (ptr + offset)
  SubF amount offset -> do
    ptr <- readMutVar pointer
    modify buffer (subtract amount) (ptr + offset)
  SetF value offset -> do
    ptr <- readMutVar pointer
    write buffer (ptr + offset) value
  MulF cell value offset -> do
    ptr <- readMutVar pointer
    x <- read buffer (ptr + offset)
    modify buffer (+ x * value) (ptr + offset + cell)
  ShiftLF amount -> do
    modifyMutVar' pointer (subtract amount)
  ShiftRF amount -> do
    modifyMutVar' pointer (+ amount)
  InputF offset -> do
    ptr <- readMutVar pointer
    modifyM buffer input (ptr + offset)
  OutputF offset -> do
    ptr <- readMutVar pointer
    value <- read buffer (ptr + offset)
    output value
  LoopF body -> do
    let body' = sequenceA_ body
        loop = do
          ptr <- readMutVar pointer
          value <- read buffer ptr
          when (value /= 0) do
            body'
            loop
    loop
