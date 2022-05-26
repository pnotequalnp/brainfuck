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
  CellSize (..),
  Optimization (..),

  -- * Parser
  parser,
  parseOptions,

  -- * Re-exports
  execParser,
) where

import Brainfuck (EofBehavior (..), Optimization (..), RuntimeSettings (..))
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

data CellSize
  = Eight
  | Sixteen
  | ThirtyTwo
  | SixtyFour

data Options = Options
  { mode :: Mode
  , cellSize :: CellSize
  , runtimeSettings :: RuntimeSettings
  , optimization :: Optimization
  , passes :: Word
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
    <*> parseCellSize
    <*> parseRuntimeSettings
    <*> parseOptimization
    <*> parsePasses
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
    , pure Interpret
    ]

parseCellSize :: Parser CellSize
parseCellSize =
  option reader $
    mconcat
      [ long "size"
      , short 's'
      , metavar "8|16|32|64"
      , help "Size of each memory cell"
      , value Eight
      , showDefaultWith \case
          Eight -> "8"
          Sixteen -> "16"
          ThirtyTwo -> "32"
          SixtyFour -> "64"
      , hidden
      ]
  where
    reader = maybeReader \case
      "8" -> Just Eight
      "16" -> Just Sixteen
      "32" -> Just ThirtyTwo
      "64" -> Just SixtyFour
      _ -> Nothing

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
      , short 'p'
      , metavar "INT"
      , help "Number of iterated optimization passes"
      , value 3
      , showDefault
      , hidden
      ]

parseUnicode :: Parser Bool
parseUnicode = not <$> switch (long "ascii" <> internal)

parseColor :: Parser Bool
parseColor = not <$> switch (long "no-color" <> internal)

parseVersion :: Parser Bool
parseVersion = switch (long "version" <> help "Print version information and exit" <> hidden)
