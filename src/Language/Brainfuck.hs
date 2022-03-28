module Language.Brainfuck
  ( Program,
    Statement,
    StatementF (..),
    compileLLVM,
    executeStatement,
    genLLVM,
    interpret,
    parse,
    renderLLVM,
    pattern ShiftL,
    pattern ShiftR,
    pattern Inc,
    pattern Dec,
    pattern Output,
    pattern Input,
    pattern Loop,
  )
where

import Language.Brainfuck.Codegen.LLVM (compileLLVM, genLLVM, renderLLVM)
import Language.Brainfuck.Interpreter (executeStatement, interpret)
import Language.Brainfuck.Parser (parse)
import Language.Brainfuck.Syntax
  ( Program,
    Statement,
    StatementF (..),
    pattern ShiftL,
    pattern ShiftR,
    pattern Inc,
    pattern Dec,
    pattern Output,
    pattern Input,
    pattern Loop,
  )
