{-# LANGUAGE AllowAmbiguousTypes #-}

{- |
 Module      : Brainfuck
 Description : Brainfuck Checks and Warnings
 Copyright   : Kevin Mullins 2022
 License     : ISC
 Maintainer  : kevin@pnotequalnp.com
-}
module Brainfuck.Checks where

import Brainfuck.Syntax
import Data.Word (Word64)
import Error.Diagnose (Report, warn)
import Foreign (Storable (..))

-- | Checks if the memory given is larger than the max value of the pointer type or zero
memorySize :: forall addr. Storable addr => Word64 -> Maybe (Report String)
memorySize memory
  | memory == 0 = Just insufficientDiag
  | memory >= pointerMax = Just redundantDiag
  | otherwise = Nothing
  where
    pointerMax = 2 ^ (sizeOf @addr undefined * 8) - 1
    redundantMsg =
      mconcat ["the maximum address is ", show pointerMax, ", but ", show memory, " memory cells are allocated"]
    redundantDiag = warn Nothing "redundant memory" [] [redundantMsg]
    insufficientDiag = warn Nothing "no memory" [] ["no memory cells are allocated"]

-- | Checks if the initial pointer is in bounds
initialPointerPosition :: forall addr. Storable addr => Word64 -> Maybe (Report String)
initialPointerPosition initialPointer
  | initialPointer >= pointerMax = Just diag
  | otherwise = Nothing
  where
    pointerMax = 2 ^ (sizeOf @addr undefined * 8) - 1
    msg =
      mconcat ["the initial pointer is ", show initialPointer, ", but ", show pointerMax, " is the maximum address"]
    diag = warn Nothing "pointer out of bounds" [] [msg]

-- | Checks if any I/O is performed during the program
heatsTheBox :: [Brainfuck byte addr] -> Maybe (Report String)
heatsTheBox program
  | any interacts program = Nothing
  | otherwise = Just diag
  where
    interacts = cata \case
      InputF _ -> True
      OutputF _ -> True
      LoopF body -> or body
      _ -> False
    diag = warn Nothing "program performs no I/O" [] []
