-- |
-- Module      : Language.Brainfuck
-- Description : Brainfuck Compiler and Interpreter
-- Copyright   : Kevin Mullins 2022
-- License     : ISC
-- Maintainer  : kevin@pnotequalnp.com
-- Stability   : unstable
--
-- = Brainfuck
-- [Brainfuck](https://esolangs.org/wiki/Brainfuck/) is a popular esolang. Its source code consists
-- only of 8 different characters, which each have a straightforward meaning. It works by moving a
-- pointer left and right over a series of memory cells. For a given cell, it can increment the
-- value in the cell, decrement the value, output the value as a character, or read a character into
-- that cell. It can also form rudimentary loops.
--
-- This module exposes a parser, interpreter, and compiler for the Brainfuck language.
module Language.Brainfuck (
  -- * Syntax
  Program,
  Statement (..),

  -- * Parser
  parse,

  -- * Interpreter
  interpret,
  executeStatement,

  -- * Compiler

  -- ** LLVM
  genLLVM,
  renderLLVM,
  compileLLVMAsm,
  compileLLVM,
) where

import Language.Brainfuck.Codegen.LLVM (compileLLVM, compileLLVMAsm, genLLVM, renderLLVM)
import Language.Brainfuck.Interpreter (executeStatement, interpret)
import Language.Brainfuck.Parser (parse)
import Language.Brainfuck.Syntax (Program, Statement (..))
