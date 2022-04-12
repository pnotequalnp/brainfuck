-- |
-- Module      : Language.Brainfuck.Syntax
-- Description : Brainfuck Abstract Syntax
-- Copyright   : Kevin Mullins 2022
-- License     : ISC
-- Maintainer  : kevin@pnotequalnp.com
-- Stability   : unstable
--
-- = Brainfuck AST and Lexemes
-- This module defines the abstract syntax of Brainfuck.
module Language.Brainfuck.Syntax (
  -- * Syntax
  Program,
  Statement (..),
  Token (..),
) where

-- | Brainfuck source lexemes.
data Token
  = -- | \<
    ArrowL
  | -- | \>
    ArrowR
  | -- | \+
    Plus
  | -- | \-
    Minus
  | -- | \.
    Dot
  | -- | \,
    Comma
  | -- | \[
    BracketL
  | -- | \]
    BracketR
  deriving stock (Eq)

-- | The Brainfuck abstract syntax tree.
data Statement
  = -- | Move the pointer to the left
    ShiftL
  | -- | Move the pointer to the right
    ShiftR
  | -- | Increment the memory cell at the pointer
    Inc
  | -- | Decrement the memory cell at the pointer
    Dec
  | -- | Output the char represented by the memory cell at the pointer
    Output
  | -- | Input a char and store it in the memory cell at the pointer
    Input
  | -- | Execute the subprogram until the current memory cell is @0@
    Loop Program

-- | A Brainfuck program consists of a sequence of `Statement`s.
type Program = [Statement]
