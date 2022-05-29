{-# LANGUAGE OverloadedStrings #-}

{- |
 Module      : Brainfuck.LLVM
 Description : Brainfuck LLVM Backend
 Copyright   : Kevin Mullins 2022
 License     : ISC
 Maintainer  : kevin@pnotequalnp.com
-}
module Brainfuck.LLVM where

import Control.Applicative (optional)
import Data.ByteString (ByteString)
import Data.IORef (newIORef, readIORef, writeIORef)
import Foreign (FunPtr, Word32, castPtrToFunPtr, wordPtrToPtr)
import LLVM (Module, moduleLLVMAssembly, moduleObject, withModuleFromAST)
import LLVM.AST qualified as AST
import LLVM.Context (withContext)
import LLVM.Linking (getSymbolAddressInProcess, loadLibraryPermanently)
import LLVM.OrcJIT (JITSymbol (..), JITSymbolFlags (..), SymbolResolver (..))
import LLVM.OrcJIT qualified as JIT
import LLVM.OrcJIT.CompileLayer qualified as JIT
import LLVM.PassManager (PassSetSpec (..), defaultCuratedPassSetSpec, runPassManager, withPassManager)
import LLVM.Target (withHostTargetMachineDefault)

-- | Compile an LLVM module to object code
compileLLVM ::
  -- | LLVM optimization level
  Word ->
  -- | LLVM AST
  AST.Module ->
  IO ByteString
compileLLVM l m = withContext \ctx ->
  withModuleFromAST ctx m \m' ->
    withHostTargetMachineDefault \tgt -> do
      _ <- optimizeLLVM l m'
      moduleObject tgt m'

-- | Pretty print an LLVM module
showLLVM ::
  -- | LLVM optimization level
  Word ->
  -- | LLVM AST
  AST.Module ->
  IO ByteString
showLLVM l m = withContext \ctx ->
  withModuleFromAST ctx m \m' -> do
    _ <- optimizeLLVM l m'
    moduleLLVMAssembly m'

-- | Execute an LLVM module with JIT compilation
jitLLVM ::
  -- | LLVM optimization level
  Word ->
  -- | LLVM AST
  AST.Module ->
  IO ()
jitLLVM l m = withContext \ctx ->
  withModuleFromAST ctx m \m' ->
    withHostTargetMachineDefault \tgt ->
      JIT.withExecutionSession \es -> do
        res <- newIORef undefined
        JIT.withObjectLinkingLayer es (\_ -> readIORef res) \linkingLayer -> do
          JIT.withIRCompileLayer linkingLayer tgt \compileLayer ->
            JIT.withModuleKey es \key -> do
              _ <- optimizeLLVM l m'
              JIT.withSymbolResolver es (SymbolResolver (resolve compileLayer)) \resolver -> do
                writeIORef res resolver
                JIT.withModule compileLayer key m' do
                  _ <- loadLibraryPermanently Nothing
                  mangled <- JIT.mangleSymbol compileLayer "main"
                  JIT.findSymbol compileLayer mangled False >>= \case
                    Left err -> error ("Internal error during JIT: " <> show err)
                    Right (JITSymbol main _) -> do
                      _ <- fromJit (castPtrToFunPtr (wordPtrToPtr main))
                      pure ()
  where
    resolve compileLayer sym =
      JIT.findSymbol compileLayer sym True >>= \case
        Left e ->
          optional (getSymbolAddressInProcess sym) >>= \case
            Nothing -> pure (Left e)
            Just ptr -> pure . Right $ JITSymbol ptr (JIT.defaultJITSymbolFlags {jitSymbolExported = True})
        Right sym' -> pure (Right sym')

-- | Optimize an LLVM module
optimizeLLVM :: Word -> Module -> IO Bool
optimizeLLVM l m = withPassManager passes \pm -> runPassManager pm m
  where
    passes = defaultCuratedPassSetSpec {optLevel = Just l}

foreign import ccall "dynamic"
  fromJit :: FunPtr (IO Word32) -> IO Word32
