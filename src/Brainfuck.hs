{- |
 Module      : Brainfuck
 Description : The Brainfuck Language
 Copyright   : Kevin Mullins 2022
 License     : ISC
 Maintainer  : kevin@pnotequalnp.com
-}
module Brainfuck (
  -- * Syntax
  Brainfuck (..),
  BrainfuckF (..),

  -- * Parsing
  parse,

  -- * Optimization
  optimize,
  Optimization (..),
  contract,
  deloopify,

  -- * Configuration
  RuntimeSettings (..),
  EofBehavior (..),

  -- * Compilation
  compile,
  compileLLVM,
  codegen,

  -- * Interpreting
  interpretIO,
  interpret,
  execute,
  Env (..),

  -- * Pretty Printing
  pretty,
  prettyIR,
) where

import Brainfuck.Configuration (EofBehavior (..), RuntimeSettings (..))
import Brainfuck.Interpreter (Env (..), execute, interpret)
import Brainfuck.Interpreter.IO (stdinInput, stdoutOutput)
import Brainfuck.LLVM (compileLLVM)
import Brainfuck.LLVM.Codegen (codegen)
import Brainfuck.Optimizer (contract, deloopify, offsetInstructions)
import Brainfuck.Parser (parse)
import Brainfuck.Syntax (Brainfuck (..), BrainfuckF (..))
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Vector.Unboxed (Unbox)
import Foreign (Storable)
import LLVM.Pretty ()
import Prettyprinter (Doc, Pretty (..), vsep)

-- | Compile a brainfuck program to object code
compile ::
  (Integral byte, Storable byte, Integral addr) =>
  -- | Runtime settings
  RuntimeSettings ->
  -- | Brainfuck program
  [Brainfuck byte addr] ->
  IO ByteString
compile memory source = compileLLVM (codegen memory source)

-- | Optimization options
data Optimization = Optimization
  { contraction :: Bool
  , deloopification :: Bool
  , offsets :: Bool
  }

-- | Run various optimization passes in sequence
optimize ::
  (Num byte, Ord byte, Num addr, Ord addr) =>
  -- | Optimization options
  Optimization ->
  -- | Brainfuck program
  [Brainfuck byte addr] ->
  [Brainfuck byte addr]
optimize Optimization {contraction, deloopification, offsets} =
  opt deloopify deloopification
    . opt offsetInstructions offsets
    . opt contract contraction
    . opt deloopify deloopification
  where
    opt = bool id

-- | Interpret in `IO`, reading from and writing to `stdin` and `stdout`
interpretIO ::
  (Num byte, Eq byte, Storable byte, Unbox byte, Integral addr) =>
  -- | Runtime settings
  RuntimeSettings ->
  -- | Brainfuck program
  [Brainfuck byte addr] ->
  IO (Env IO byte)
interpretIO RuntimeSettings {memory, eofBehavior} =
  interpret (stdinInput eofBehavior) stdoutOutput memory

-- | Pretty print brainfuck IR
prettyIR :: (Pretty byte, Pretty addr, Eq addr, Num addr) => [Brainfuck byte addr] -> Doc ann
prettyIR = vsep . fmap pretty
