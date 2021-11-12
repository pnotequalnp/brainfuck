module Main where

import Control.Monad.Loops (whileM_)
import Control.Monad.State (evalStateT, get, liftIO, modify)
import Data.Char (chr, ord)
import Data.Functor ((<&>))
import Data.Vector.Mutable qualified as V
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import Text.Read (readMaybe)

data Brainfuck
  = ShiftL
  | ShiftR
  | Inc
  | Dec
  | Output
  | Input
  | Loop [Brainfuck]
  deriving Show

main :: IO ()
main = do
  memory <- getArgs >>= \case
    "-m" : (readMaybe -> Just x) : _ -> pure x
    _ -> pure 30_000
  getContents <&> parseBrainfuck >>= \case
    Nothing -> hPutStrLn stderr "Invalid syntax"
    Just prog -> runBrainfuck prog memory

runBrainfuck :: [Brainfuck] -> Int -> IO ()
runBrainfuck prog memSize = do
  memory <- V.replicate memSize (0 :: Int)
  let run = \case
        [] -> pure ()
        ShiftL : prog' -> do
          modify (subtract 1)
          run prog'
        ShiftR : prog' -> do
          modify (+ 1)
          run prog'
        Inc : prog' -> do
          get >>= V.modify memory (+ 1)
          run prog'
        Dec : prog' -> do
          get >>= V.modify memory (subtract 1)
          run prog'
        Output : prog' -> do
          x <- get >>= V.read memory
          liftIO . putChar . chr $ x
          run prog'
        Input : prog' -> do
          c <- liftIO getChar
          get >>= flip (V.write memory) (ord c)
          run prog'
        Loop inner : prog' -> do
          whileM_ (get >>= V.read memory <&> (/= 0)) $ run inner
          run prog'

  (`evalStateT` (0 :: Int)) $ run prog

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
