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
  Set x off : Add y off' : xs | off == off' -> contract (Set (x + y) off : xs)
  Add x off : Add y off' : xs | off == off' -> contract (Add (x + y) off : xs)
  Shift x : Shift y : xs -> contract (Shift (x + y) : xs)
  Shift 0 : xs -> contract xs
  x : xs -> x : contract xs

-- | Replace common loop idioms with single instructions
deloopify :: (Num byte, Eq byte, Num addr, Eq addr) => [Brainfuck byte addr] -> [Brainfuck byte addr]
deloopify = \case
  [] -> []
  Loop [Add 1 off] : xs -> Set 0 off : deloopify xs
  Loop [Set x off] : xs -> Set x off : deloopify xs
  Loop [Loop body] : xs -> deloopify (Loop body : xs)
  Loop body : xs | Just (reset, mults) <- multLoop body -> deloopify (mults <> (reset : xs))
  Loop body : xs ->
    if body == body'
      then Loop body : deloopify xs
      else deloopify (Loop body' : xs)
    where
      body' = deloopify body
  x : xs -> x : deloopify xs
  where
    multLoop = \case
      Add n off : xs | off /= 0 -> fmap (Mul n off 0 :) <$> multLoop xs
      Dec : xs -> (Set 0 0,) <$> multLoop' xs
      _ -> Nothing
    multLoop' = \case
      Add n off : xs | off /= 0 -> (Mul n off 0 :) <$> multLoop' xs
      [] -> pure []
      _ -> Nothing

-- | Avoid redundant pointer manipulation by doing operations at an offset from the pointer
offsetInstructions :: (Num addr, Ord addr) => [Brainfuck byte addr] -> [Brainfuck byte addr]
offsetInstructions = \case
  [] -> []
  Loop body : xs -> Loop (offsetInstructions body) : offsetInstructions xs
  Shift off : x : Shift off' : xs
    | Just x' <- offset off x -> x' : offsetInstructions (Shift (off + off') : xs)
  x : xs -> x : offsetInstructions xs
  where
    offset shift = \case
      Add x off -> Just (Add x (shift + off))
      Set x off -> Just (Set x (shift + off))
      Input off -> Just (Input (shift + off))
      Output off -> Just (Output (shift + off))
      _ -> Nothing
