-- |
-- Module      : Language.Brainfuck.Parser
-- Description : Brainfuck Parser
-- Copyright   : Kevin Mullins 2022
-- License     : ISC
-- Maintainer  : kevin@pnotequalnp.com
-- Stability   : unstable
--
-- = Brainfuck Parser
-- This module contains a lexer and a parser for the Brainfuck language.
module Language.Brainfuck.Parser (
  -- * Parsing
  parse,
) where

import Control.Applicative (asum, many)
import Control.Monad.State (StateT (..), evalStateT)
import Data.Functor (($>))
import Data.Maybe (mapMaybe)
import Language.Brainfuck.Syntax (Program, Statement (..), Token (..))
import Prelude hiding (lex)

-- | Lex a @String@ into a list of `Token`s. This function always succeeds (unless the input is
-- undefined).
lex :: String -> [Token]
lex = mapMaybe \case
  '<' -> Just ArrowL
  '>' -> Just ArrowR
  '+' -> Just Plus
  '-' -> Just Minus
  '.' -> Just Dot
  ',' -> Just Comma
  '[' -> Just BracketL
  ']' -> Just BracketR
  _ -> Nothing

-- | Parse a @String@ into a Brainfuck AST. Failure is caused by unmatched brackets.
parse :: String -> Maybe Program
parse = evalStateT program . lex

type Parser = StateT [Token] Maybe

program :: Parser Program
program = many expr <* eof

expr :: Parser Statement
expr =
  asum
    [ token ArrowL $> ShiftL
    , token ArrowR $> ShiftR
    , token Plus $> Inc
    , token Minus $> Dec
    , token Dot $> Output
    , token Comma $> Input
    , token Comma $> Input
    , Loop <$> loop
    ]

loop :: Parser [Statement]
loop = do
  token BracketL
  body <- many expr
  token BracketR
  pure body

token :: Token -> Parser ()
token t = StateT \case
  t' : ts | t == t' -> Just ((), ts)
  _ -> Nothing

eof :: Parser ()
eof = StateT \case
  [] -> Just ((), [])
  _ -> Nothing
