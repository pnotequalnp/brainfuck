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

  -- * Compilation
  compile,
  compileLLVM,
  codegen,

  -- * Pretty Printing
  pretty,
  prettyIR,
) where

import Brainfuck.LLVM (compileLLVM)
import Brainfuck.LLVM.Codegen (codegen)
import Brainfuck.Optimizer (contract, deloopify)
import Brainfuck.Parser (parse)
import Brainfuck.Syntax (Brainfuck (..), BrainfuckF (..))
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Word (Word64)
import LLVM.Pretty ()
import Prettyprinter (Doc, Pretty (..), vsep)

-- | Compile a Brainfuck program to object code
compile ::
  (Integral byte, Integral addr) =>
  -- | Memory size in bytes
  Word64 ->
  -- | Brainfuck program
  [Brainfuck byte addr] ->
  IO ByteString
compile memory source = compileLLVM (codegen memory source)

-- | Optimization options
data Optimization = Optimization
  { contraction :: Bool
  , deloopification :: Bool
  }

-- | Run various optimization passes in sequence
optimize ::
  (Num byte, Ord byte, Num addr, Ord addr) =>
  -- | Optimization options
  Optimization ->
  -- | Brainfuck program
  [Brainfuck byte addr] ->
  [Brainfuck byte addr]
optimize Optimization {contraction, deloopification} =
  opt contract contraction
    . opt deloopify deloopification
  where
    opt = bool id

-- | Pretty print Brainfuck IR
prettyIR :: (Pretty byte, Pretty addr, Eq addr, Num addr) => [Brainfuck byte addr] -> Doc ann
prettyIR = vsep . fmap pretty
