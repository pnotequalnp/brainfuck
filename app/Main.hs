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
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Version (showVersion)
import Error.Diagnose (defaultStyle, printDiagnostic)
import Paths_brainfuck (version)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (replaceExtension, takeExtension)
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
      Eight -> parse @Int8 @Int fp source >>= run
      Sixteen -> parse @Int16 @Int fp source >>= run
      ThirtyTwo -> parse @Int32 @Int fp source >>= run
      SixtyFour -> parse @Int64 @Int fp source >>= run
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
          Execute -> BF.jitLLVM (BF.codegen runtimeSettings source)
          Compile -> do
            binary <- BF.compile runtimeSettings source
            case outputFile of
              Nothing
                | Just fp <- sourceFile
                  , takeExtension fp /= ".o" ->
                  BS.writeFile (replaceExtension fp ".o") binary
              Nothing -> BS.putStrLn binary
              Just fp -> BS.writeFile fp binary
          DumpIR -> case outputFile of
            Nothing -> print (BF.showIR source)
            Just fp -> writeFile fp (show (BF.showIR source))
          DumpLLVM -> do
            llvm <- BF.showLLVM (BF.codegen runtimeSettings source)
            case outputFile of
              Nothing -> BS.putStr llvm
              Just fp -> BS.writeFile fp llvm
