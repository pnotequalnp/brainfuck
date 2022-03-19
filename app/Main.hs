module Main (main) where

import Control.Applicative (asum, optional)
import Data.Foldable (fold)
import Data.Version (showVersion)
import Language.Brainfuck qualified as BF
import Options.Applicative (Parser, ParserInfo)
import Options.Applicative qualified as Opts
import Paths_brainfuck (version)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

data Mode
  = Interpret
  | Version

data Options = Options
  { mode :: Mode,
    filePath :: Maybe FilePath,
    memory :: Word
  }

main :: IO ()
main = do
  Options {mode, filePath, memory} <- Opts.execParser parser
  program <- BF.parse <$> maybe getContents readFile filePath
  case mode of
    Version -> putStrLn (showVersion version)
    Interpret -> case program of
      Nothing -> hPutStrLn stderr "Invalid syntax" *> exitFailure
      Just program' -> BF.interpret memory program'

parser :: ParserInfo Options
parser =
  Opts.info (Opts.helper <*> parseOptions) $
    fold
      [ Opts.fullDesc,
        Opts.header ("Brainfuck " <> showVersion version),
        Opts.footer "https://github.com/pnotequalnp/brainfuck"
      ]

parseOptions :: Parser Options
parseOptions = Options <$> parseMode <*> optional parseFilePath <*> parseMemory

parseFilePath :: Parser FilePath
parseFilePath = Opts.strArgument (Opts.metavar "FILEPATH")

parseMode :: Parser Mode
parseMode =
  asum
    [ Opts.flag' Version (Opts.long "version" <> Opts.short 'v' <> Opts.help "Print Brainfuck version"),
      Opts.flag' Interpret (Opts.long "exec" <> Opts.short 'x' <> Opts.help "Interpret a Brainfuck program"),
      pure Interpret
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
