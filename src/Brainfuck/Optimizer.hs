{- |
 Module      : Brainfuck.Optimizer
 Description : Brainfuck Optimizations
 Copyright   : Kevin Mullins 2022
 License     : ISC
 Maintainer  : kevin@pnotequalnp.com
-}
module Brainfuck.Optimizer where

import Brainfuck.Syntax

-- | Contract chains of operations like `Add` and `ShiftR` into single combined operations
contract :: (Num byte, Ord byte, Num addr, Ord addr) => [Brainfuck byte addr] -> [Brainfuck byte addr]
contract = \case
  [] -> []
  Loop body : xs -> Loop (contract body) : contract xs
  Add _ off : Set x off' : xs | off == off' -> contract (Set x off' : xs)
  Sub _ off : Set x off' : xs | off == off' -> contract (Set x off' : xs)
  Set x off : Add y off' : xs | off == off' -> contract (Set (x + y) off : xs)
  Set x off : Sub y off' : xs | off == off' -> contract (Set (x - y) off : xs)
  Add x off : Add y off' : xs | off == off' -> contract (Add (x + y) off : xs)
  Sub x off : Sub y off' : xs | off == off' -> contract (Sub (x + y) off : xs)
  Add x off : Sub y off' : xs | off == off' -> case compare x y of
    LT -> contract (Sub (y - x) off : xs)
    EQ -> contract xs
    GT -> contract (Add (x - y) off : xs)
  Sub x off : Add y off' : xs | off == off' -> case compare x y of
    LT -> contract (Add (y - x) off : xs)
    EQ -> contract xs
    GT -> contract (Sub (x - y) off : xs)
  ShiftR x : ShiftR y : xs -> contract (ShiftR (x + y) : xs)
  ShiftL x : ShiftL y : xs -> contract (ShiftL (x + y) : xs)
  ShiftR x : ShiftL y : xs -> case compare x y of
    LT -> contract (ShiftL (y - x) : xs)
    EQ -> contract xs
    GT -> contract (ShiftR (x - y) : xs)
  ShiftL x : ShiftR y : xs -> case compare x y of
    LT -> contract (ShiftR (y - x) : xs)
    EQ -> contract xs
    GT -> contract (ShiftL (x - y) : xs)
  x : xs -> x : contract xs

-- | Replace common loop idioms with single instructions
deloopify :: (Num byte, Eq byte, Eq addr) => [Brainfuck byte addr] -> [Brainfuck byte addr]
deloopify = \case
  [] -> []
  Loop [Add 1 off] : xs -> Set 0 off : deloopify xs
  Loop [Sub 1 off] : xs -> Set 0 off : deloopify xs
  Loop [Set x off] : xs -> Set x off : deloopify xs
  Loop [Loop body] : xs -> deloopify (Loop body : xs)
  Loop body : xs -> case body == body' of
    False -> deloopify (Loop body' : xs)
    True -> Loop body : deloopify xs
    where
      body' = deloopify body
  x : xs -> x : deloopify xs
