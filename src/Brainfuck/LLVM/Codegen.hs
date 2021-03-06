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
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans (lift)
import Data.ByteString.Short (ShortByteString)
import Data.Foldable (sequenceA_, traverse_)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Word (Word32)
import Foreign (Storable (..))
import LLVM.AST (Module, Name (..), Operand (..), mkName)
import LLVM.AST.AddrSpace (AddrSpace (..))
import LLVM.AST.Constant (Constant (Int))
import LLVM.AST.IntegerPredicate qualified as Pred
import LLVM.AST.Type (Type (..), i32, i64)
import LLVM.IRBuilder

type Codegen s = IRBuilderT (ModuleBuilderT (ST s))

-- | Generate an LLVM module from a brainfuck program
codegen ::
  forall byte addr.
  (Integral byte, Storable byte, Integral addr, Storable addr) =>
  -- | Runtime settings
  RuntimeSettings ->
  -- | Brainfuck program
  [Brainfuck byte addr] ->
  Module
codegen RuntimeSettings {memory, initialPointer, eofBehavior} source = runST $ buildModuleT "main" do
  pointer <- lift . newSTRef $ addrLiteral (fromIntegral initialPointer)
  getchar <- extern (mkName "getchar") [] i32
  putchar <- extern (mkName "putchar") [i32] i32
  memset <- extern (mkName "memset") [cellPointer, i32, i64] cellPointer
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
    _ <- namedBlock "entry"
    buffer <- alloca (ArrayType memory cellType) Nothing cellWidth `named` "buffer"
    let bufferBytes = fromIntegral cellBytes * fromIntegral memory
    _ <- call memset [(buffer, []), (int32 0, []), (int64 bufferBytes, [])]
    traverse_ (statement getbyte putbyte literal addrLiteral cellWidth (bufferOffset buffer pointer) pointer) source
    ret (int32 0)
  pure ()
  where
    cellBytes = fromIntegral (sizeOf @byte undefined)
    cellWidth = cellBytes * 8
    cellType = IntegerType cellWidth
    cellPointer = PointerType cellType (AddrSpace 0)
    literal = ConstantOperand . Int cellWidth . toInteger
    pointerWidth = fromIntegral (sizeOf @addr undefined) * 8
    pointerType = IntegerType pointerWidth
    addrLiteral = ConstantOperand . Int pointerWidth . toInteger
    bufferOffset buffer pointer x = do
      addr <- lift . lift . readSTRef $ pointer
      addr' <- case x of
        0 -> pure addr
        _ -> addSub addrLiteral addr x
      gep buffer [int64 0, addr']

statement ::
  (Integral byte, Integral addr) =>
  -- | Input codegen
  (Operand -> Codegen s ()) ->
  -- | Output codegen
  (Operand -> Codegen s ()) ->
  -- | Construct a literal value
  (byte -> Operand) ->
  -- | Construct a literal address
  (addr -> Operand) ->
  -- | Width of each cell
  Word32 ->
  -- | Get address of cell in buffer at an offset
  (addr -> Codegen s Operand) ->
  -- | Pointer
  STRef s Operand ->
  -- | Brainfuck instruction
  Brainfuck byte addr ->
  Codegen s ()
statement input output literal addrLiteral cellWidth bufferOffset pointer = cata \case
  AddF amount offset -> do
    loc <- bufferOffset offset
    x <- load loc cellWidth
    x' <- addSub literal x amount
    store loc cellWidth x'
  SetF value offset -> do
    loc <- bufferOffset offset
    store loc cellWidth (literal value)
  MulF value cell offset -> do
    src <- bufferOffset offset
    dest <- bufferOffset (offset + cell)
    x <- load src cellWidth
    y <- mul x (literal value)
    z <- load dest cellWidth
    z' <- add y z
    store dest cellWidth z'
  ShiftF amount -> do
    addr <- getPointer
    addr' <- addSub addrLiteral addr amount
    putPointer addr'
  InputF offset -> do
    loc <- bufferOffset offset
    input loc
  OutputF offset -> do
    loc <- bufferOffset offset
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
    loc <- bufferOffset 0
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
  NopF -> pure ()
  where
    getPointer = lift . lift $ readSTRef pointer
    putPointer = lift . lift . writeSTRef pointer

namedBlock :: ShortByteString -> Codegen s Name
namedBlock name = do
  emitBlockStart name'
  pure name'
  where
    name' = Name name

addSub :: forall a m. (MonadIRBuilder m, Num a, Ord a) => (a -> Operand) -> Operand -> a -> m Operand
addSub toOp y x
  | x < 0 = sub y (toOp (-x)) -- signed <0
  | -x >= x = add y (toOp x) -- unsigned >=0
  | (-1 :: a) < 0 = add y (toOp x) -- signed >=0
  | otherwise = sub y (toOp (-x)) -- unsigned <0
