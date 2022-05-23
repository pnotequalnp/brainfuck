{- |
 Module      : Main
 Description : Brainfuck Compiler and Interpreter
 Copyright   : Kevin Mullins 2022
 License     : ISC
 Maintainer  : kevin@pnotequalnp.com
-}
module Main (main) where

import Brainfuck (Brainfuck)
import Brainfuck qualified as BF
import Brainfuck.Options
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Version (showVersion)
import Data.Word (Word8)
import Error.Diagnose (defaultStyle, printDiagnostic)
import Paths_brainfuck (version)
import System.Exit (die, exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  Options {mode, sourceFile, outputFile, memory, optimization, unicode, color} <- execParser parser
  let getSource :: IO [Brainfuck Word8 Int]
      getSource = do
        (fp, src) <- case sourceFile of
          Just fp | fp /= "-" -> (fp,) <$> LBS.readFile fp
          _ -> ("<STDIN>",) <$> LBS.getContents
        case BF.parse fp src of
          Right source -> pure (BF.optimize optimization source)
          Left diag -> do
            printDiagnostic stderr unicode color 2 defaultStyle diag
            exitFailure
  case mode of
    Interpret -> do
      source <- getSource
      _ <- BF.interpretIO memory source
      pure ()
    Execute -> die "not implemented"
    Compile -> do
      source <- getSource
      binary <- BF.compile memory source
      case outputFile of
        Nothing -> BS.putStrLn binary
        Just fp -> BS.writeFile fp binary
    DumpIR -> do
      source <- getSource
      case outputFile of
        Nothing -> print (BF.prettyIR source)
        Just fp -> writeFile fp (show (BF.prettyIR source))
    DumpLLVM -> do
      source <- getSource
      let llvmIR = BF.pretty (BF.codegen memory source)
      case outputFile of
        Nothing -> print llvmIR
        Just fp -> writeFile fp (show llvmIR)
    Version -> hPutStrLn stderr (showVersion version)
