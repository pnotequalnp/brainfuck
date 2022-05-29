{-# LANGUAGE ApplicativeDo #-}
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
  Width (..),
  Optimization (..),

  -- * Parser
  parser,
  parseOptions,

  -- * Re-exports
  execParser,
) where

import Brainfuck (EofBehavior (..), Optimization (..), RuntimeSettings (..))
import Control.Monad (when)
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

data Width = W8 | W16 | W32 | W64

data Options = Options
  { mode :: Mode
  , cellWidth :: Width
  , pointerWidth :: Width
  , runtimeSettings :: RuntimeSettings
  , optimization :: Optimization
  , passes :: Word
  , optLevel :: Word
  , unicode :: Bool
  , color :: Bool
  , outputFile :: Maybe FilePath
  , sourceFile :: Maybe FilePath
  , printVersion :: Bool
  }

parser :: ParserInfo Options
parser =
  info (parseOptions <**> helper) $
    mconcat
      [ fullDesc
      , header ("brainfuck " <> showVersion version)
      , footer "https://github.com/pnotequalnp/brainfuck"
      ]

parseOptions :: Parser Options
parseOptions =
  Options
    <$> parseMode
    <*> parseCellWidth
    <*> parsePointerWidth
    <*> parseRuntimeSettings
    <*> parseOptimization
    <*> parsePasses
    <*> parseOptLevel
    <*> parseUnicode
    <*> parseColor
    <*> parseOutput
    <*> parseSource
    <*> parseVersion

parseMode :: Parser Mode
parseMode =
  asum
    [ flag' Interpret (long "interpret" <> short 'i' <> help "Interpret source file directly")
    , flag' Compile (long "compile" <> short 'c' <> help "Compile source file")
    , flag' DumpIR (long "dump-ir" <> help "Output IR from source file" <> hidden)
    , flag' DumpLLVM (long "dump-llvm" <> help "Output LLVM IR from source file" <> hidden)
    , pure Execute
    ]

parseCellWidth :: Parser Width
parseCellWidth =
  option reader $
    mconcat
      [ long "width"
      , short 'w'
      , metavar "8|16|32|64"
      , help "Width of each memory cell"
      , value W8
      , showDefaultWith (const "8")
      , hidden
      ]
  where
    reader =
      auto @Word >>= \case
        8 -> pure W8
        16 -> pure W16
        32 -> pure W32
        64 -> pure W64
        _ -> fail "invalid cell width"

parsePointerWidth :: Parser Width
parsePointerWidth =
  option reader $
    mconcat
      [ long "pointer"
      , short 'p'
      , metavar "8|16|32|64"
      , help "Width of the pointer"
      , value W64
      , showDefaultWith (const "64")
      , hidden
      ]
  where
    reader =
      auto @Word >>= \case
        8 -> pure W8
        16 -> pure W16
        32 -> pure W32
        64 -> pure W64
        _ -> fail "invalid pointer width"

parseRuntimeSettings :: Parser RuntimeSettings
parseRuntimeSettings = do
  posMem <- parseMemory
  initialPointer <- parseNegativeMemory
  eofBehavior <- parseEof
  pure
    RuntimeSettings
      { memory = posMem + initialPointer
      , initialPointer
      , eofBehavior
      }

parseMemory :: Parser Word64
parseMemory =
  option auto $
    mconcat
      [ long "memory"
      , short 'm'
      , metavar "INT"
      , help "Number of (positive) memory cells"
      , value 30000
      , showDefault
      , hidden
      ]

parseNegativeMemory :: Parser Word64
parseNegativeMemory =
  option auto $
    mconcat
      [ long "negative"
      , short 'n'
      , metavar "INT"
      , help "Number of negative memory cells"
      , value 0
      , showDefault
      , hidden
      ]

parseEof :: Parser EofBehavior
parseEof =
  option reader $
    mconcat
      [ long "eof"
      , help "Behavior on EOF"
      , value Unchanged
      , showDefaultWith \case
          Zero -> "0"
          Unchanged -> "unchanged"
          NegativeOne -> "-1"
      , metavar "unchanged|0|-1"
      , hidden
      ]
  where
    reader = maybeReader \case
      "unchanged" -> Just Unchanged
      "0" -> Just Zero
      "-1" -> Just NegativeOne
      _ -> Nothing

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
        , offsets = True
        }

parseOptimizations :: Parser Optimization
parseOptimizations =
  Optimization
    <$> switch (long "contract" <> help "Contract `+`/`-` and `<`/`>` chains to single instructions" <> hidden)
    <*> switch (long "deloopify" <> help "Reduce `[-]`-like loops to single instructions" <> hidden)
    <*> switch (long "offset" <> help "Perform operations at offsets from the pointer" <> hidden)

parsePasses :: Parser Word
parsePasses =
  option auto $
    mconcat
      [ long "passes"
      , metavar "INT"
      , help "Number of iterated optimization passes"
      , value 3
      , showDefault
      , hidden
      ]

parseOptLevel :: Parser Word
parseOptLevel =
  option reader $
    mconcat
      [ long "optlevel"
      , metavar "0-3"
      , help "LLVM optimization level"
      , value 0
      , showDefault
      , hidden
      ]
  where
    reader = do
      x <- auto @Word
      when (x > 3) do
        fail "invalid optimization level"
      pure x

parseUnicode :: Parser Bool
parseUnicode = not <$> switch (long "ascii" <> internal)

parseColor :: Parser Bool
parseColor = not <$> switch (long "no-color" <> internal)

parseVersion :: Parser Bool
parseVersion = switch (long "version" <> help "Print version information and exit" <> hidden)
