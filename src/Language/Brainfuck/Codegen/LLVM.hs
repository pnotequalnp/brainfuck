module Language.Brainfuck.Codegen.LLVM where

import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Text.Lazy qualified as L
import LLVM.AST (Module (..), Operand, Type (..), defaultModule, mkName)
import LLVM.IRBuilder
import LLVM.Pretty (ppllvm)
import Language.Brainfuck.Syntax

genLLVM :: Program -> Module
genLLVM program =
  defaultModule
    { moduleDefinitions = defs
    }
  where
    defs = execModuleBuilder emptyModuleBuilder do
      getbyte <- extern (mkName "getbyte") [] (IntegerType 32)
      putbyte <- extern (mkName "putbyte") [IntegerType 32] (IntegerType 32)
      _ <- function (mkName "main") [] (IntegerType 32) \_ -> do
        _ <- block
        buffer <- alloca (ArrayType 30_000 (IntegerType 8)) Nothing 8
        let ctx = Context {buffer, putbyte, getbyte}
        (`runReaderT` ctx) . (`evalStateT` (int64 0)) $ traverse_ buildIR program
      pure ()

data Context = Context
  { buffer :: Operand,
    putbyte :: Operand,
    getbyte :: Operand
  }

buildIR ::
  (MonadIRBuilder m, MonadModuleBuilder m, MonadReader Context m, MonadState Operand m) =>
  Statement ->
  m ()
buildIR = \case
  ShiftL -> do
    ptr <- get
    ptr' <- sub ptr (int64 1)
    put ptr'
  ShiftR -> do
    ptr <- get
    ptr' <- add ptr (int64 1)
    put ptr'
  Inc -> do
    buffer <- asks (.buffer)
    loc <- gep buffer ([])
    x <- load loc 8
    x' <- add x (int8 1)
    store loc 8 x'
  Dec -> do
    buffer <- asks (.buffer)
    loc <- gep buffer ([])
    x <- load loc 8
    x' <- sub x (int8 1)
    store loc 8 x'
  Output -> do
    putbyte <- asks (.putbyte)
    buffer <- asks (.buffer)
    loc <- gep buffer ([])
    x8 <- load loc 8
    x32 <- sext x8 (IntegerType 32)
    _ <- call putbyte [(x32, [])]
    pure ()
  Input -> do
    getbyte <- asks (.getbyte)
    buffer <- asks (.buffer)
    loc <- gep buffer ([])
    x32 <- call getbyte []
    x8 <- trunc x32 (IntegerType 8)
    store loc 8 x8
  Loop _statements -> do
    ptr <- get
    prev <- currentBlock
    cur <- block
    _ <- phi [(ptr, prev), (undefined, cur)]
    error "loop"

renderLLVM :: Module -> L.Text
renderLLVM = ppllvm

compileLLVM :: Module -> IO ByteString
compileLLVM = error "LLVM compilation not implemented"
