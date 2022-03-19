module Language.Brainfuck.Codegen.LLVM where

import Data.ByteString (ByteString)
import LLVM.AST (Module)
import Language.Brainfuck.Syntax (Program)

genLLVM :: Program -> Module
genLLVM = error "LLVM generation not implemented"

renderLLVM :: Module -> IO ByteString
renderLLVM = error "LLVM pretty printing not implemented"

compileLLVM :: Module -> IO ByteString
compileLLVM = error "LLVM compilation not implemented"
