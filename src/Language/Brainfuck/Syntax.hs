module Language.Brainfuck.Syntax where

data Token
  = ArrowL
  | ArrowR
  | Plus
  | Minus
  | Dot
  | Comma
  | BracketL
  | BracketR
  deriving stock (Eq)

data Statement
  = ShiftL
  | ShiftR
  | Inc
  | Dec
  | Output
  | Input
  | Loop [Statement]

type Program = [Statement]
