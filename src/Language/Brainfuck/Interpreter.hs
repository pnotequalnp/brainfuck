module Language.Brainfuck.Interpreter where

import Control.Monad.Loops (whileM_)
import Control.Monad.State (StateT, evalStateT, get, liftIO, modify)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.Vector.Unboxed.Mutable (IOVector)
import Data.Vector.Unboxed.Mutable qualified as V
import Data.Word (Word8)
import Language.Brainfuck.Interpreter.Internal (getByte, putByte)
import Language.Brainfuck.Syntax

type Byte = Word8

interpret :: Word -> Program -> IO ()
interpret memSize program = do
  memory <- V.replicate (fromIntegral memSize) 0
  (`evalStateT` 0) $ traverse_ (executeStatement memory) program

executeStatement :: IOVector Byte -> Statement -> StateT Int IO ()
executeStatement memory = \case
  ShiftL -> modify (- 1)
  ShiftR -> modify (+ 1)
  Inc -> get >>= V.modify memory (+ 1)
  Dec -> get >>= V.modify memory (- 1)
  Output -> do
    x <- get >>= V.read memory
    liftIO (putByte x)
  Input -> do
    x <- liftIO getByte
    get >>= flip (V.write memory) x
  Loop statements ->
    whileM_ (get >>= V.read memory <&> (/= 0)) $
      traverse_ (executeStatement memory) statements
