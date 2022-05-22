{-# LANGUAGE StrictData #-}

{- |
 Module      : Brainfuck.Options
 Description : Brainfuck CLI Option Parser
 Copyright   : Kevin Mullins 2022
 License     : ISC
 Maintainer  : kevin@pnotequalnp.com
-}
module Brainfuck.Options (
  -- * Options
  Options (..),
  Mode (..),
  Optimization (..),

  -- * Parser
  parser,
  parseOptions,

  -- * Re-exports
  execParser,
) where

import Brainfuck (Optimization (..))
import Data.Foldable (asum)
import Data.Version (showVersion)
import Data.Word (Word64)
import Options.Applicative
import Paths_brainfuck (version)

data Mode
  = Interpret
  | Execute
  | Compile
  | DumpIR
  | DumpLLVM
  | Version

data Options = Options
  { mode :: Mode
  , memory :: Word64
  , optimization :: Optimization
  , unicode :: Bool
  , color :: Bool
  , outputFile :: Maybe FilePath
  , sourceFile :: Maybe FilePath
  }

parser :: ParserInfo Options
parser =
  info (parseOptions <**> helper) $
    mconcat
      [ fullDesc
      , header ("Brainfuck " <> showVersion version)
      , footer "https://github.com/pnotequalnp/brainfuck"
      ]

parseOptions :: Parser Options
parseOptions =
  Options
    <$> parseMode
    <*> parseMemory
    <*> parseOptimization
    <*> parseUnicode
    <*> parseColor
    <*> parseOutput
    <*> parseSource

parseMode :: Parser Mode
parseMode =
  asum
    [ flag' Interpret (long "interpret" <> short 'i' <> help "Interpret source file directly")
    , flag' Compile (long "compile" <> short 'c' <> help "Compile source file")
    , flag' DumpIR (long "dump-ir" <> help "Output IR from source file" <> hidden)
    , flag' DumpLLVM (long "dump-llvm" <> help "Output LLVM IR from source file" <> hidden)
    , flag' Version (long "version" <> help "Print version information and exit" <> hidden)
    , pure Execute
    ]

parseMemory :: Parser Word64
parseMemory =
  option auto $
    mconcat
      [ long "memory"
      , short 'm'
      , metavar "BYTES"
      , help "Memory size in bytes"
      , value 30000
      , showDefault
      , hidden
      ]

parseSource :: Parser (Maybe FilePath)
parseSource = optional (strArgument (metavar "SOURCE_FILE"))

parseOutput :: Parser (Maybe FilePath)
parseOutput = optional (strOption (short 'o' <> metavar "OUTPUT_FILE" <> hidden))

parseOptimization :: Parser Optimization
parseOptimization =
  flag' defOpts (short 'O' <> help "Enable all optimizations")
    <|> parseOptimizations
  where
    defOpts =
      Optimization
        { contraction = True
        , deloopification = True
        }

parseOptimizations :: Parser Optimization
parseOptimizations =
  Optimization
    <$> switch (long "contract" <> help "Contract `+`/`-` and `<`/`>` chains to single instructions" <> hidden)
    <*> switch (long "deloopify" <> help "Reduce `[-]`-like loops to single instructions" <> hidden)

parseUnicode :: Parser Bool
parseUnicode = not <$> switch (long "ascii" <> internal)

parseColor :: Parser Bool
parseColor = not <$> switch (long "no-color" <> internal)
