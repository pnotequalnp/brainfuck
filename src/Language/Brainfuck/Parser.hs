module Language.Brainfuck.Parser where

import Control.Applicative (asum, many)
import Control.Monad.State (StateT (..), evalStateT)
import Data.Functor (($>))
import Data.Maybe (mapMaybe)
import Language.Brainfuck.Syntax
import Prelude hiding (lex)

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

type Parser = StateT [Token] Maybe

parse :: String -> Maybe Program
parse = evalStateT program . lex

program :: Parser Program
program = many expr <* eof

expr :: Parser Statement
expr =
  asum
    [ token ArrowL $> ShiftL,
      token ArrowR $> ShiftR,
      token Plus $> Inc,
      token Minus $> Dec,
      token Dot $> Output,
      token Comma $> Input,
      token Comma $> Input,
      Loop <$> loop
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
