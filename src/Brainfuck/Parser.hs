{- |
 Module      : Brainfuck.Parser
 Description : Brainfuck Parser
 Copyright   : Kevin Mullins 2022
 License     : ISC
 Maintainer  : kevin@pnotequalnp.com
-}
module Brainfuck.Parser (
  -- * Parsing
  Brainfuck.Parser.parse,
) where

import Brainfuck.Syntax
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Word (Word8)
import Error.Diagnose (Diagnostic, Marker (..), Position (..), addFile, addReport, def, err)
import Text.Megaparsec
import Text.Megaparsec.Byte (char)

data MismatchedBracket
  = UnmatchedOpen SourcePos
  | UnmatchedClose SourcePos
  deriving stock (Eq, Ord, Show)

type Parser = Parsec MismatchedBracket ByteString

-- | Parse a Brainfuck program from a bytestring
parse ::
  (Num byte, Eq byte, Num addr, Eq addr) =>
  FilePath ->
  ByteString ->
  Either (Diagnostic String) [Brainfuck byte addr]
parse fp source = case Text.Megaparsec.parse brainfuck fp source of
  Right res -> Right res
  Left ParseErrorBundle {bundleErrors = FancyError _ errs :| []}
    | [ErrorCustom e] <- toList errs ->
      Left . addSource $ addReport def case e of
        UnmatchedOpen p ->
          err Nothing "unmatched open bracket" [(posToPosition p, This "unmatched bracket")] []
        UnmatchedClose p ->
          err Nothing "unmatched closing bracket" [(posToPosition p, This "unmatched bracket")] []
  Left ParseErrorBundle {bundlePosState = PosState {pstateSourcePos}} ->
    Left . addSource . addReport def $
      err Nothing "unknown parse error" [(posToPosition pstateSourcePos, This "parse error")] []
  where
    addSource diag = addFile diag fp (unpack source)
    posToPosition SourcePos {sourceName, sourceLine, sourceColumn} =
      Position
        { file = sourceName
        , begin = (unPos sourceLine, unPos sourceColumn)
        , end = (unPos sourceLine, unPos sourceColumn + 1)
        }

brainfuck :: (Eq byte, Eq addr, Num byte, Num addr) => Parser [Brainfuck byte addr]
brainfuck = do
  _ <- comment
  stmts <- many statement
  eof <|> do
    p <- getSourcePos
    customFailure (UnmatchedClose p)
  pure stmts

statement :: (Eq byte, Eq addr, Num byte, Num addr) => Parser (Brainfuck byte addr)
statement =
  choice
    [ Inc <$ sym 43 -- +
    , Dec <$ sym 45 -- -
    , Backward <$ sym 60 -- <
    , Forward <$ sym 62 -- >
    , In <$ sym 44 -- ,
    , Out <$ sym 46 -- .
    , Loop <$> do
        p <- getSourcePos
        _ <- sym 91 -- [
        body <- many statement
        body <$ sym 93 -- ]
          <|> customFailure (UnmatchedOpen p)
    ]

commentByte :: Word8 -> Bool
commentByte x =
  x /= 43 -- +
    && x /= 45 -- -
    && x /= 60 -- <
    && x /= 62 -- >
    && x /= 44 -- ,
    && x /= 46 -- .
    && x /= 91 -- [
    && x /= 93 -- ]

comment :: Parser ByteString
comment = takeWhileP Nothing commentByte

sym :: Word8 -> Parser Word8
sym x = char x <* comment
