{- |
 Module      : Brainfuck.LLVM
 Description : Brainfuck LLVM Backend
 Copyright   : Kevin Mullins 2022
 License     : ISC
 Maintainer  : kevin@pnotequalnp.com
-}
module Brainfuck.LLVM where

import Data.ByteString (ByteString)
import LLVM (Module, moduleObject, withModuleFromAST)
import LLVM.AST qualified as AST (Module)
import LLVM.Context (withContext)
import LLVM.PassManager (PassSetSpec (..), defaultCuratedPassSetSpec, runPassManager, withPassManager)
import LLVM.Target (withHostTargetMachineDefault)

-- | Compile an LLVM module to object code
compileLLVM :: AST.Module -> IO ByteString
compileLLVM m = withContext \ctx ->
  withModuleFromAST ctx m \m' ->
    withHostTargetMachineDefault \tgt -> do
      _ <- optimizeLLVM m'
      moduleObject tgt m'

-- | Optimize an LLVM module
optimizeLLVM :: Module -> IO Bool
optimizeLLVM m = withPassManager passes \pm -> runPassManager pm m
  where
    passes = defaultCuratedPassSetSpec {optLevel = Just 1}
