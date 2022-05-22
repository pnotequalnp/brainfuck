{-# LANGUAGE OverloadedStrings #-}

{- |
 Module      : Brainfuck.LLVM.Codegen
 Description : Brainfuck LLVM Code Generation
 Copyright   : Kevin Mullins 2022
 License     : ISC
 Maintainer  : kevin@pnotequalnp.com
-}
module Brainfuck.LLVM.Codegen (
  -- * Codegen
  codegen,
) where

import Brainfuck.Syntax
import Control.Monad (join)
import Control.Monad.Reader (ReaderT (..), asks, lift)
import Control.Monad.ST (ST, runST)
import Data.Foldable (sequenceA_, traverse_)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Word (Word64)
import LLVM.AST (Module, Operand, mkName)
import LLVM.AST.IntegerPredicate qualified as Pred
import LLVM.AST.Type (Type (..))
import LLVM.IRBuilder

-- | Generate an LLVM module from a Brainfuck program
codegen :: (Integral byte, Integral addr) => Word64 -> [Brainfuck byte addr] -> Module
codegen memory source = runST $ buildModuleT "main" do
  pointer <- lift $ newSTRef (int64 0)
  getchar <- extern (mkName "getchar") [] (IntegerType 32)
  putchar <- extern (mkName "putchar") [IntegerType 32] (IntegerType 32)
  let getbyte = mdo
        br entry
        entry <- block
        x <- call getchar []
        cmp <- icmp Pred.SLT x (int32 0)
        condBr cmp exit success
        success <- block
        x' <- trunc x (IntegerType 8)
        br exit
        exit <- block
        phi [(x', success), (int8 0, entry)]
      putbyte x = do
        x' <- sext x (IntegerType 32)
        _ <- call putchar [(x', [])]
        pure ()
  _ <- function (mkName "main") [] (IntegerType 32) \_ -> do
    buffer <- alloca (ArrayType memory (IntegerType 8)) Nothing 8
    let ctx = Context {buffer, pointer, getbyte, putbyte}
    (`runReaderT` ctx) $ traverse_ statement source
    ret (int32 0)
  pure ()

data Context s = Context
  { buffer :: Operand
  , pointer :: STRef s Operand
  , getbyte :: Codegen s Operand
  , putbyte :: Operand -> Codegen s ()
  }

type Codegen s = ReaderT (Context s) (IRBuilderT (ModuleBuilderT (ST s)))

statement :: (Integral byte, Integral addr) => Brainfuck byte addr -> Codegen s ()
statement = cata \case
  AddF amount _offset -> do
    loc <- currentCell
    x <- load loc 8
    x' <- add x (toByte amount)
    store loc 8 x'
  SubF amount _offset -> do
    loc <- currentCell
    x <- load loc 8
    x' <- sub x (toByte amount)
    store loc 8 x'
  SetF value _offset -> do
    loc <- currentCell
    store loc 8 (toByte value)
  ShiftLF amount -> do
    addr <- getPointer
    addr' <- sub addr (toPtr amount)
    putPointer addr'
  ShiftRF amount -> do
    addr <- getPointer
    addr' <- add addr (toPtr amount)
    putPointer addr'
  InputF _offset -> do
    loc <- currentCell
    x <- join (asks getbyte)
    store loc 8 x
  OutputF _offset -> do
    output <- asks putbyte
    loc <- currentCell
    x <- load loc 8
    output x
  LoopF instructions -> mdo
    prevBlock <- currentBlock
    prevPtr <- getPointer
    br loop

    loop <- block `named` "loop_head"
    loopPtr <- phi [(prevPtr, prevBlock), (bodyPtr, bodyEnd)]
    putPointer loopPtr
    loc <- currentCell
    x <- load loc 8
    zero <- icmp Pred.EQ x (int8 0)
    condBr zero end body

    body <- block `named` "loop_body"
    sequenceA_ instructions
    bodyPtr <- getPointer
    bodyEnd <- currentBlock
    br loop

    end <- block `named` "loop_end"
    pure ()

getPointer :: Codegen s Operand
getPointer = asks pointer >>= lift . lift . lift . readSTRef

putPointer :: Operand -> Codegen s ()
putPointer x = asks pointer >>= lift . lift . lift . (`writeSTRef` x)

toPtr :: Integral addr => addr -> Operand
toPtr = int64 . toInteger

toByte :: Integral byte => byte -> Operand
toByte = int8 . toInteger

currentCell :: Codegen s Operand
currentCell = do
  addr <- getPointer
  buffer <- asks buffer
  gep buffer ([int64 0, addr])
