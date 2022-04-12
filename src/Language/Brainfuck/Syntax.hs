module Language.Brainfuck.Syntax where

import Data.Fix (Fix (..))

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

data StatementF a
  = ShiftLF
  | ShiftRF
  | IncF
  | DecF
  | OutputF
  | InputF
  | LoopF [a]
  deriving stock (Functor)

type Statement = Fix StatementF

pattern ShiftL :: Statement
pattern ShiftL = Fix ShiftLF

pattern ShiftR :: Statement
pattern ShiftR = Fix ShiftRF

pattern Inc :: Statement
pattern Inc = Fix IncF

pattern Dec :: Statement
pattern Dec = Fix DecF

pattern Output :: Statement
pattern Output = Fix OutputF

pattern Input :: Statement
pattern Input = Fix InputF

pattern Loop :: [Statement] -> Statement
pattern Loop xs = Fix (LoopF xs)

{-# COMPLETE ShiftL, ShiftR, Inc, Dec, Output, Input, Loop #-}

type Program = [Statement]
