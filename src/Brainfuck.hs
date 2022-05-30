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

  -- * Checks
  checks,

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
  jitLLVM,
  compileLLVM,
  optimizeLLVM,
  codegen,

  -- * Interpreting
  interpretIO,
  interpret,
  execute,

  -- * Pretty Printing
  showIR,
  showLLVM,
  showASM,
) where

import Brainfuck.Checks (heatsTheBox, memorySize)
import Brainfuck.Configuration (EofBehavior (..), RuntimeSettings (..))
import Brainfuck.Interpreter (execute, interpret)
import Brainfuck.Interpreter.IO (handleInput, handleOutput)
import Brainfuck.LLVM (compileLLVM, jitLLVM, optimizeLLVM, showASM, showLLVM)
import Brainfuck.LLVM.Codegen (codegen)
import Brainfuck.Optimizer (contract, deloopify, offsetInstructions)
import Brainfuck.Parser (parse)
import Brainfuck.Syntax (Brainfuck (..), BrainfuckF (..))
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Foldable (foldl')
import Data.Maybe (catMaybes)
import Data.Vector.Unboxed.Mutable (IOVector, Unbox)
import Error.Diagnose (Diagnostic, addReport, def)
import Foreign (Storable)
import Prettyprinter (Doc, Pretty (..), vsep)
import System.IO (Handle)

-- | Compile a brainfuck program to object code
compile ::
  (Integral byte, Storable byte, Integral addr, Storable addr) =>
  -- | Runtime settings
  RuntimeSettings ->
  -- | LLVM optimization level
  Word ->
  -- | Brainfuck program
  [Brainfuck byte addr] ->
  IO ByteString
compile settings optLevel source = compileLLVM optLevel (codegen settings source)

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
  (Num byte, Eq byte, Storable byte, Unbox byte, Integral addr) =>
  -- | Input handle
  Handle ->
  -- | Output handle
  Handle ->
  -- | Runtime settings
  RuntimeSettings ->
  -- | Brainfuck program
  [Brainfuck byte addr] ->
  IO (IOVector byte, addr)
interpretIO hIn hOut RuntimeSettings {memory, initialPointer, eofBehavior} =
  interpret (handleInput hIn eofBehavior) (handleOutput hOut) (fromIntegral memory) (fromIntegral initialPointer)

-- | Pretty print brainfuck IR
showIR :: (Pretty byte, Eq byte, Num byte, Pretty addr, Eq addr, Num addr) => [Brainfuck byte addr] -> Doc ann
showIR = vsep . fmap pretty

-- | Checks and warnings
checks ::
  forall byte addr.
  Storable addr =>
  -- | Runtime settings
  RuntimeSettings ->
  -- | Brainfuck program
  [Brainfuck byte addr] ->
  Maybe (Diagnostic String)
checks RuntimeSettings {memory} program = case warnings of
  [] -> Nothing
  _ -> Just (foldl' addReport def warnings)
  where
    warnings =
      catMaybes
        [ heatsTheBox program
        , memorySize @addr memory
        ]
