module Language.Brainfuck
  ( Program,
    Statement (..),
    executeStatement,
    interpret,
    parse,
  )
where

import Language.Brainfuck.Interpreter (executeStatement, interpret)
import Language.Brainfuck.Parser (parse)
import Language.Brainfuck.Syntax (Program, Statement (..))
