module Main where

import Control.Monad.Loops (whileM_)
import Control.Monad.State (evalStateT, get, liftIO, modify)
import Data.Char (chr, ord)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.Vector.Primitive.Mutable qualified as V
import Data.Word (Word8)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

data Brainfuck
  = ShiftL
  | ShiftR
  | Inc
  | Dec
  | Output
  | Input
  | Loop [Brainfuck]
  deriving (Show)

main :: IO ()
main = do
  memory <-
    getArgs >>= \case
      "-m" : (readMaybe -> Just x) : _ -> pure x
      _ -> pure 30_000
  getContents <&> parseBrainfuck >>= \case
    Nothing -> hPutStrLn stderr "Invalid syntax"
    Just prog -> runBrainfuck prog memory

runBrainfuck :: [Brainfuck] -> Int -> IO ()
runBrainfuck prog memSize = do
  memory <- V.replicate memSize (0 :: Word8)
  let run = \case
        ShiftL -> modify (subtract 1)
        ShiftR -> modify (+ 1)
        Inc -> get >>= V.modify memory (+ 1)
        Dec -> get >>= V.modify memory (subtract 1)
        Output -> do
          x <- get >>= V.read memory
          liftIO . putChar . chr . fromIntegral $ x
        Input -> do
          c <- liftIO getChar
          get >>= flip (V.write memory) (fromIntegral $ ord c)
        Loop inner ->
          whileM_ (get >>= V.read memory <&> (/= 0)) $ traverse_ run inner

  (`evalStateT` (0 :: Int)) $ traverse_ run prog

parseBrainfuck :: String -> Maybe [Brainfuck]
parseBrainfuck = open
  where
    open = \case
      [] -> Just []
      '[' : s -> case close s of
        Nothing -> Nothing
        Just (s', Loop -> inner) -> (inner :) <$> open s'
      (instr -> Just x) : s -> (x :) <$> open s
      _ -> Nothing

    close = \case
      ']' : s -> Just (s, [])
      (instr -> Just x) : s -> fmap (x :) <$> close s
      '[' : s -> case close s of
        Nothing -> Nothing
        Just (s', Loop -> inner) -> fmap (inner :) <$> close s'
      _ -> Nothing

    instr = \case
      '<' -> Just ShiftL
      '>' -> Just ShiftR
      '+' -> Just Inc
      '-' -> Just Dec
      '.' -> Just Output
      ',' -> Just Input
      _ -> Nothing
