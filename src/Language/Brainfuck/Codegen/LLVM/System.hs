module Language.Brainfuck.Codegen.LLVM.System where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Lazy.Encoding (encodeUtf8)
import LLVM.AST (Module)
import LLVM.Pretty (ppllvm)
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed

llvmToAsm :: Module -> IO ByteString
llvmToAsm llvm = readProcessStdout_ $ setStdin ((byteStringInput . encodeUtf8 . ppllvm) llvm) "llc"

asmToBinary :: ByteString -> IO ByteString
asmToBinary asm = withSystemTempDirectory "brainfuck" \dir -> do
  let out = dir <> "/binary"
  runProcess_ . setStdin (byteStringInput asm) $ proc "clang" ["-x", "assembler", "-o", out, "-"]
  LBS.readFile out
