{- |
 Module      : Brainfuck.LLVM
 Description : Brainfuck LLVM Backend
 Copyright   : Kevin Mullins 2022
 License     : ISC
 Maintainer  : kevin@pnotequalnp.com
-}
module Brainfuck.LLVM where

import Data.ByteString (ByteString)
import LLVM (moduleObject, withModuleFromAST)
import LLVM.AST (Module)
import LLVM.Context (withContext)
import LLVM.Target (withHostTargetMachineDefault)

-- | Compile an LLVM module to object code
compileLLVM :: Module -> IO ByteString
compileLLVM m = withContext \ctx ->
  withModuleFromAST ctx m \m' ->
    withHostTargetMachineDefault \tgt ->
      moduleObject tgt m'
