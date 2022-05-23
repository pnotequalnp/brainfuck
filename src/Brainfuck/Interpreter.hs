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
import Control.Monad.Reader (ReaderT (..), ask, lift)
import Data.Foldable (sequenceA_, traverse_)
import Data.Primitive (modifyMutVar', newMutVar)
import Data.Primitive.MutVar (MutVar, readMutVar)
import Data.Vector.Unboxed.Mutable (MVector, Unbox, modify, modifyM, new, read, write)
import Data.Word (Word64)
import Prelude hiding (read)

-- | Runtime environment for interpreting brainfuck
data Env m byte = Env
  { buffer :: MVector (PrimState m) byte
  , pointer :: MutVar (PrimState m) Int
  , input :: byte -> m byte
  , output :: byte -> m ()
  }

-- | Interpret a brainfuck program
interpret ::
  (PrimMonad m, Num byte, Eq byte, Unbox byte, Integral addr) =>
  -- | Input function
  (byte -> m byte) ->
  -- | Output function
  (byte -> m ()) ->
  -- | Memory size in bytes
  Word64 ->
  -- | Brainfuck program
  [Brainfuck byte addr] ->
  m (Env m byte)
interpret input output memory program = do
  buffer <- new (fromIntegral memory)
  pointer <- newMutVar 0
  let env = Env {buffer, pointer, input, output}
  _ <- runReaderT (traverse_ execute program) env
  pure env

-- | Execute a single brainfuck instruction
execute ::
  (PrimMonad m, Num byte, Eq byte, Unbox byte, Integral addr) =>
  Brainfuck byte addr ->
  ReaderT (Env m byte) m ()
execute = cata \case
  AddF amount offset -> do
    Env {buffer, pointer} <- ask
    ptr <- readMutVar pointer
    modify buffer (+ amount) (ptr + fromIntegral offset)
  SubF amount offset -> do
    Env {buffer, pointer} <- ask
    ptr <- readMutVar pointer
    modify buffer (subtract amount) (ptr + fromIntegral offset)
  SetF value offset -> do
    Env {buffer, pointer} <- ask
    ptr <- readMutVar pointer
    write buffer (ptr + fromIntegral offset) value
  MulF cell value offset -> do
    Env {buffer, pointer} <- ask
    ptr <- readMutVar pointer
    x <- read buffer (ptr + fromIntegral offset)
    modify buffer (+ x * value) (ptr + fromIntegral (offset + cell))
  ShiftLF amount -> do
    Env {pointer} <- ask
    modifyMutVar' pointer (subtract (fromIntegral amount))
  ShiftRF amount -> do
    Env {pointer} <- ask
    modifyMutVar' pointer (+ fromIntegral amount)
  InputF offset -> do
    Env {buffer, pointer, input} <- ask
    ptr <- readMutVar pointer
    modifyM buffer (lift . input) (ptr + fromIntegral offset)
  OutputF offset -> do
    Env {buffer, pointer, output} <- ask
    ptr <- readMutVar pointer
    value <- read buffer (ptr + fromIntegral offset)
    lift (output value)
  LoopF body -> do
    Env {buffer, pointer} <- ask
    let body' = sequenceA_ body
        loop = do
          ptr <- readMutVar pointer
          value <- read buffer ptr
          when (value /= 0) do
            body'
            loop
    loop
