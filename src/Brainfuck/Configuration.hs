{- |
 Module      : Brainfuck.Configuration
 Description : Brainfuck Runtime Settings
 Copyright   : Kevin Mullins 2022
 License     : ISC
 Maintainer  : kevin@pnotequalnp.com
-}
module Brainfuck.Configuration (
  RuntimeSettings (..),
  EofBehavior (..),
) where

import Data.Word (Word64)

-- | Settings for brainfuck runtime behavior
data RuntimeSettings = RuntimeSettings
  { -- | Number of memory cells available
    memory :: Word64
  , -- | Initial location of the pointer
    initialPointer :: Word64
  , -- | Behavior when the input stream has reached EOF
    eofBehavior :: EofBehavior
  }

-- | Action to take when the input stream reaches EOF
data EofBehavior
  = -- | Return @0@
    Zero
  | -- | Leave the cell unchanged
    Unchanged
  | -- | Return @-1@
    NegativeOne
