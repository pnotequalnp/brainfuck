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
import Control.Monad (when)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Version (showVersion)
import Data.Word (Word16, Word32, Word64, Word8)
import Error.Diagnose (defaultStyle, printDiagnostic)
import Paths_brainfuck (version)
import System.Exit (die, exitFailure, exitSuccess)
import System.FilePath (replaceExtension)
import System.IO (hPutStrLn, stderr, stdin, stdout)

main :: IO ()
main = do
  opts <- execParser parser
  main' opts

main' :: Options -> IO ()
main'
  Options
    { mode
    , sourceFile
    , outputFile
    , cellSize
    , runtimeSettings
    , optimization
    , passes
    , unicode
    , color
    , printVersion
    } = do
    when printVersion do
      hPutStrLn stderr (showVersion version)
      exitSuccess
    (fp, source) <- case sourceFile of
      Just fp | fp /= "-" -> (fp,) <$> LBS.readFile fp
      _ -> ("<STDIN>",) <$> LBS.getContents
    case cellSize of
      Eight -> parse @Word8 @Int fp source >>= run
      Sixteen -> parse @Word16 @Int fp source >>= run
      ThirtyTwo -> parse @Word32 @Int fp source >>= run
      SixtyFour -> parse @Word64 @Int fp source >>= run
    where
      parse :: (Num byte, Num addr, Ord byte, Ord addr) => FilePath -> ByteString -> IO [Brainfuck byte addr]
      parse fp src =
        case BF.parse fp src of
          Right source -> pure (BF.optimize optimization passes source)
          Left diag -> do
            printDiagnostic stderr unicode color 2 defaultStyle diag
            exitFailure
      run source =
        case mode of
          Interpret -> do
            _ <- BF.interpretIO stdin stdout runtimeSettings source
            pure ()
          Execute -> die "not implemented"
          Compile -> do
            binary <- BF.compile runtimeSettings source
            case outputFile of
              Nothing | Just fp <- sourceFile -> BS.writeFile (replaceExtension fp ".o") binary
              Nothing -> BS.putStrLn binary
              Just fp -> BS.writeFile fp binary
          DumpIR -> case outputFile of
            Nothing -> print (BF.prettyIR source)
            Just fp -> writeFile fp (show (BF.prettyIR source))
          DumpLLVM -> do
            let llvmIR = BF.pretty (BF.codegen runtimeSettings source)
            case outputFile of
              Nothing -> print llvmIR
              Just fp -> writeFile fp (show llvmIR)
