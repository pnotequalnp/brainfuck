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

import Brainfuck.Configuration (EofBehavior (..), RuntimeSettings (..))
import Brainfuck.Syntax
import Control.Monad.Reader (ReaderT (..), asks, lift)
import Control.Monad.ST (ST, runST)
import Data.ByteString.Short (ShortByteString)
import Data.Foldable (sequenceA_, traverse_)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Foreign (Storable (..))
import LLVM.AST (Module, Name (..), Operand (ConstantOperand), mkName)
import LLVM.AST.Constant (Constant (..))
import LLVM.AST.IntegerPredicate qualified as Pred
import LLVM.AST.Type (Type (..), i32)
import LLVM.IRBuilder

-- | Generate an LLVM module from a brainfuck program
codegen ::
  forall byte addr.
  (Integral byte, Storable byte, Integral addr) =>
  -- | Runtime settings
  RuntimeSettings ->
  -- | Brainfuck program
  [Brainfuck byte addr] ->
  Module
codegen RuntimeSettings {memory, eofBehavior} source = runST $ buildModuleT "main" do
  pointer <- lift $ newSTRef (int64 0)
  getchar <- extern (mkName "getchar") [] i32
  putchar <- extern (mkName "putchar") [i32] i32
  let getbyte loc = mdo
        br entry
        entry <- block
        x <- call getchar []
        cmp <- icmp Pred.SLT x (int32 0)
        condBr cmp failure success
        success <- block
        x' <- case compare 32 cellWidth of
          LT -> sext x cellType
          EQ -> pure x
          GT -> trunc x cellType
        store loc cellWidth x'
        br exit
        failure <- block
        case eofBehavior of
          Zero -> store loc cellWidth (literal 0)
          Unchanged -> pure ()
          NegativeOne -> store loc cellWidth (literal (-1))
        br exit
        exit <- block
        pure ()
      putbyte x = do
        x' <- case compare 32 cellWidth of
          LT -> trunc x i32
          EQ -> pure x
          GT -> sext x i32
        _ <- call putchar [(x', [])]
        pure ()
  _ <- function (mkName "main") [] i32 \_ -> do
    _ <- block `named` "entry"
    buffer <- alloca (ArrayType memory cellType) Nothing cellWidth `named` "buffer"
    let ctx = Context {buffer, pointer, getbyte, putbyte}
    (`runReaderT` ctx) $ traverse_ statement source
    ret (int32 0)
  pure ()
  where
    cellWidth = fromIntegral (sizeOf @byte undefined) * 8
    cellType = IntegerType cellWidth
    literal = ConstantOperand . Int cellWidth
    toPtr = int64 . toInteger
    toByte = literal . toInteger
    statement = cata \case
      AddF amount _offset -> do
        loc <- currentCell
        x <- load loc cellWidth
        x' <- add x (toByte amount)
        store loc cellWidth x'
      SubF amount _offset -> do
        loc <- currentCell
        x <- load loc cellWidth
        x' <- sub x (toByte amount)
        store loc cellWidth x'
      SetF value _offset -> do
        loc <- currentCell
        store loc cellWidth (toByte value)
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
        input <- asks getbyte
        input loc
      OutputF _offset -> do
        output <- asks putbyte
        loc <- currentCell
        x <- load loc cellWidth
        output x
      LoopF loopBody -> mdo
        prevBlock <- currentBlock
        prevPtr <- getPointer
        br loop

        Name name <- freshName "loop"
        loop <- namedBlock (name <> "_head")
        loopPtr <- phi [(prevPtr, prevBlock), (bodyPtr, bodyEnd)]
        putPointer loopPtr
        loc <- currentCell
        x <- load loc cellWidth
        zero <- icmp Pred.EQ x (literal 0)
        condBr zero end body

        body <- namedBlock (name <> "_body")
        sequenceA_ loopBody
        bodyPtr <- getPointer
        bodyEnd <- currentBlock
        br loop

        end <- namedBlock (name <> "_end")
        putPointer loopPtr

data Context s = Context
  { buffer :: Operand
  , pointer :: STRef s Operand
  , getbyte :: Operand -> Codegen s ()
  , putbyte :: Operand -> Codegen s ()
  }

type Codegen s = ReaderT (Context s) (IRBuilderT (ModuleBuilderT (ST s)))

getPointer :: Codegen s Operand
getPointer = asks pointer >>= lift . lift . lift . readSTRef

putPointer :: Operand -> Codegen s ()
putPointer x = asks pointer >>= lift . lift . lift . (`writeSTRef` x)

currentCell :: Codegen s Operand
currentCell = do
  addr <- getPointer
  buffer <- asks buffer
  gep buffer [int64 0, addr]

namedBlock :: ShortByteString -> Codegen s Name
namedBlock name = do
  emitBlockStart name'
  pure name'
  where
    name' = Name name
