module Main (main) where

import Control.Applicative (asum, optional)
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (fold)
import Data.Text.Lazy.IO qualified as L
import Data.Version (showVersion)
import Language.Brainfuck qualified as BF
import Options.Applicative (Parser, ParserInfo)
import Options.Applicative qualified as Opts
import Paths_brainfuck (version)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

data Mode
  = Interpret
  | Compile Stage Backend
  | Version

data Backend
  = LLVM

data Stage
  = Binary
  | Assembly
  | IR

data Options = Options
  { filePath :: Maybe FilePath,
    memory :: Word,
    outputFile :: Maybe FilePath,
    mode :: Mode
  }

main :: IO ()
main = do
  Options {mode, filePath, outputFile, memory} <- Opts.execParser parser
  parsed <- BF.parse <$> maybe getContents readFile filePath
  let outputLBS = maybe LBS.putStr LBS.writeFile outputFile
      outputText = maybe L.putStr L.writeFile outputFile
      getProgram = maybe (hPutStrLn stderr "Invalid syntax" *> exitFailure) pure parsed
  case mode of
    Version -> putStrLn (showVersion version)
    Interpret -> getProgram >>= BF.interpret memory
    Compile stage backend -> do
      program <- getProgram
      case backend of
        LLVM -> case stage of
          IR -> outputText (BF.renderLLVM llvm)
          Assembly -> BF.compileLLVMAsm llvm >>= outputLBS
          Binary -> BF.compileLLVM llvm >>= outputLBS
          where
            llvm = BF.genLLVM program

parser :: ParserInfo Options
parser =
  Opts.info (Opts.helper <*> parseOptions) $
    fold
      [ Opts.fullDesc,
        Opts.header ("Brainfuck " <> showVersion version),
        Opts.footer "https://github.com/pnotequalnp/brainfuck"
      ]

parseOptions :: Parser Options
parseOptions = Options <$> optional parseFilePath <*> parseMemory <*> optional parseFilePath <*> parseMode

parseFilePath :: Parser FilePath
parseFilePath = Opts.strArgument (Opts.metavar "FILEPATH")

parseMode :: Parser Mode
parseMode =
  asum
    [ Opts.flag' Version (Opts.long "version" <> Opts.short 'v' <> Opts.help "Print Brainfuck version"),
      Opts.flag' Interpret (Opts.long "exec" <> Opts.short 'x' <> Opts.help "Interpret a Brainfuck program"),
      Compile <$> parseStage <*> parseBackend
    ]

parseMemory :: Parser Word
parseMemory =
  Opts.option Opts.auto $
    fold
      [ Opts.long "memory",
        Opts.short 'm',
        Opts.metavar "BYTES",
        Opts.help "Memory size in bytes",
        Opts.value 30_000,
        Opts.showDefault
      ]

parseStage :: Parser Stage
parseStage =
  asum
    [ Opts.flag' IR (Opts.long "emit-ir" <> Opts.help "Dump intermediate representation"),
      Opts.flag' Assembly (Opts.long "emit-asm" <> Opts.help "Dump assembly"),
      pure Binary
    ]

parseBackend :: Parser Backend
parseBackend =
  asum
    [ Opts.flag' LLVM (Opts.long "llvm" <> Opts.help "Compile via LLVM"),
      pure LLVM
    ]
