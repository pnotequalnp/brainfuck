module Language.Brainfuck
  ( Program,
    Statement (..),
    compileLLVM,
    executeStatement,
    genLLVM,
    interpret,
    parse,
    renderLLVM,
  )
where

import Language.Brainfuck.Codegen.LLVM (compileLLVM, genLLVM, renderLLVM)
import Language.Brainfuck.Interpreter (executeStatement, interpret)
import Language.Brainfuck.Parser (parse)
import Language.Brainfuck.Syntax (Program, Statement (..))
