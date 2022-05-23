{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{- |
 Module      : Brainfuck.Syntax
 Description : Brainfuck Abstract Syntax
 Copyright   : Kevin Mullins 2022
 License     : ISC
 Maintainer  : kevin@pnotequalnp.com
-}
module Brainfuck.Syntax (
  -- * Syntax
  Brainfuck (.., Inc, Dec, Backward, Forward, In, Out),
  BrainfuckF (.., IncF, DecF, BackwardF, ForwardF, InF, OutF),

  -- * Re-exports
  module Data.Functor.Foldable,
) where

import Data.Functor.Foldable
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Prettyprinter (Pretty (..), hsep, nest, parens, vsep)

-- | Brainfuck language intermediate representation
data Brainfuck byte addr
  = -- | Add the given value to the memory cell at the given offset from the pointer
    Add byte addr
  | -- | Subtract the given value from the memory cell at the given offset from the pointer
    Sub byte addr
  | -- | Set the memory cell at the given offset from the pointer to the given value
    Set byte addr
  | -- | Multiply the current offset memory cell and add the result to the combined offset cell
    Mul addr byte addr
  | -- | Shift the pointer to the left by some offset
    ShiftL addr
  | -- | Shift the pointer to the right by some offset
    ShiftR addr
  | -- | Input a byte into the cell at the given offset from the pointer
    Input addr
  | -- | Output the byte at the given offset from the pointer
    Output addr
  | -- | Execute the subprogram until the current memory cell is @0@
    Loop [Brainfuck byte addr]
  deriving stock (Eq, Ord, Show)

-- | Increment the memory cell at the pointer
pattern Inc :: (Eq byte, Eq addr, Num byte, Num addr) => Brainfuck byte addr
pattern Inc = Add 1 0

-- | Decrement the memory cell at the pointer
pattern Dec :: (Eq byte, Eq addr, Num byte, Num addr) => Brainfuck byte addr
pattern Dec = Sub 1 0

-- | Move the pointer to the left
pattern Backward :: (Eq addr, Num addr) => Brainfuck byte addr
pattern Backward = ShiftL 1

-- | Move the pointer to the right
pattern Forward :: (Eq addr, Num addr) => Brainfuck byte addr
pattern Forward = ShiftR 1

-- | Input a char and store it in the memory cell at the pointer
pattern In :: (Eq addr, Num addr) => Brainfuck byte addr
pattern In = Input 0

-- | Output the char represented by the memory cell at the pointer
pattern Out :: (Eq addr, Num addr) => Brainfuck byte addr
pattern Out = Output 0

-- | Base functor for `Brainfuck`, for use with recursion schemes
makeBaseFunctor ''Brainfuck

pattern IncF :: (Eq byte, Eq addr, Num byte, Num addr) => BrainfuckF byte addr a
pattern IncF = AddF 1 0

pattern DecF :: (Eq byte, Eq addr, Num byte, Num addr) => BrainfuckF byte addr a
pattern DecF = SubF 1 0

pattern BackwardF :: (Eq addr, Num addr) => BrainfuckF byte addr a
pattern BackwardF = ShiftLF 1

pattern ForwardF :: (Eq addr, Num addr) => BrainfuckF byte addr a
pattern ForwardF = ShiftRF 1

pattern InF :: (Eq addr, Num addr) => BrainfuckF byte addr a
pattern InF = InputF 0

pattern OutF :: (Eq addr, Num addr) => BrainfuckF byte addr a
pattern OutF = OutputF 0

instance (Pretty byte, Pretty addr, Eq addr, Num addr) => Pretty (Brainfuck byte addr) where
  pretty = \case
    Add x 0 -> hsep ["Add", pretty x]
    Add x off -> hsep ["Add", pretty x, parens (pretty off)]
    Sub x 0 -> hsep ["Sub", pretty x]
    Sub x off -> hsep ["Sub", pretty x, parens (pretty off)]
    Set x 0 -> hsep ["Set", pretty x]
    Set x off -> hsep ["Set", pretty x, parens (pretty off)]
    Mul x y 0 -> hsep ["Mul", pretty x, pretty y]
    Mul x y off -> hsep ["Mul", pretty x, pretty y, parens (pretty off)]
    ShiftL x -> hsep ["Left", pretty x]
    ShiftR x -> hsep ["Right", pretty x]
    Input 0 -> "Input"
    Input off -> hsep ["Input", parens (pretty off)]
    Output 0 -> "Output"
    Output off -> hsep ["Output", parens (pretty off)]
    Loop body -> nest 2 . vsep $ "Loop:" : (pretty <$> body)
