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
  optimizeLLVM,
  codegen,

  -- * Interpreting
  interpretIO,
  interpret,
  execute,

  -- * Pretty Printing
  pretty,
  prettyIR,
) where

import Brainfuck.Configuration (EofBehavior (..), RuntimeSettings (..))
import Brainfuck.Interpreter (execute, interpret)
import Brainfuck.Interpreter.IO (handleInput, handleOutput)
import Brainfuck.LLVM (compileLLVM, optimizeLLVM)
import Brainfuck.LLVM.Codegen (codegen)
import Brainfuck.Optimizer (contract, deloopify, offsetInstructions)
import Brainfuck.Parser (parse)
import Brainfuck.Syntax (Brainfuck (..), BrainfuckF (..))
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Vector.Unboxed.Mutable (IOVector, Unbox)
import Foreign (Storable)
import LLVM.Pretty ()
import Prettyprinter (Doc, Pretty (..), vsep)
import System.IO (Handle)

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
  -- | Number of passes
  Word ->
  -- | Brainfuck program
  [Brainfuck byte addr] ->
  [Brainfuck byte addr]
optimize Optimization {contraction, deloopification, offsets} = composeN pass
  where
    opt = bool id
    pass =
      opt offsetInstructions offsets
        . opt contract contraction
        . opt deloopify deloopification
    composeN f = \case
      0 -> id
      n -> f . composeN f (n - 1)

-- | Interpret in `IO`, reading from and writing to `stdin` and `stdout`
interpretIO ::
  (Num byte, Eq byte, Storable byte, Unbox byte) =>
  -- | Input handle
  Handle ->
  -- | Output handle
  Handle ->
  -- | Runtime settings
  RuntimeSettings ->
  -- | Brainfuck program
  [Brainfuck byte Int] ->
  IO (IOVector byte, Int)
interpretIO hIn hOut RuntimeSettings {memory, initialPointer, eofBehavior} =
  interpret (handleInput hIn eofBehavior) (handleOutput hOut) (fromIntegral memory) (fromIntegral initialPointer)

-- | Pretty print brainfuck IR
prettyIR :: (Pretty byte, Eq byte, Num byte, Pretty addr, Eq addr, Num addr) => [Brainfuck byte addr] -> Doc ann
prettyIR = vsep . fmap pretty
