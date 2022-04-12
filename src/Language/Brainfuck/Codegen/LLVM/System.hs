-- |
-- Module      : Language.Brainfuck.Codegen.LLVM.System
-- Description : Compiling LLVM IR via external processes
-- Copyright   : Kevin Mullins 2022
-- License     : ISC
-- Maintainer  : kevin@pnotequalnp.com
-- Stability   : unstable
--
-- = LLVM Compilation
-- This module compiles LLVM IR to assembly via @llc@ and assembly to binary via @clang@.
module Language.Brainfuck.Codegen.LLVM.System (
  -- * LLVM IR Compilation
  llvmToAsm,

  -- * Assembling
  asmToBinary,
) where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Lazy.Encoding (encodeUtf8)
import LLVM.AST (Module)
import LLVM.Pretty (ppllvm)
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (byteStringInput, proc, readProcessStdout_, runProcess_, setStdin)

-- | Compile LLVM IR to assembly via @llc@.
llvmToAsm :: Module -> IO ByteString
llvmToAsm llvm = readProcessStdout_ $ setStdin ((byteStringInput . encodeUtf8 . ppllvm) llvm) "llc"

-- | Compile assembly to binary via @clang@.
asmToBinary :: ByteString -> IO ByteString
asmToBinary asm = withSystemTempDirectory "brainfuck" \dir -> do
  let out = dir <> "/binary"
  runProcess_ . setStdin (byteStringInput asm) $ proc "clang" ["-x", "assembler", "-o", out, "-"]
  LBS.readFile out
