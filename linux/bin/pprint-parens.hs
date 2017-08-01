#!/usr/bin/env runhaskell

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Data.List (elemIndex)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

usage = "Usage: pprint-parens.hs [--indent-size n] [--max-width n]"

main = do
  parseConfig <$> getArgs >>= \case
    Left err -> do
      hPutStrLn stderr err
      hPutStrLn stderr usage
      exitFailure
    Right config -> do
      contents <- getContents
      putStrLn $ pprintParens config contents

data Config = Config
  { configIndentSize :: Int
  , configMaxWidth :: Int
  }

defaultConfig = Config
  { configIndentSize = 2
  , configMaxWidth = 80
  }

parseConfig :: [String] -> Either String Config
parseConfig = parse $ defaultConfig
  where
  parse :: Config -> [String] -> Either String Config
  parse Config{..} args = case args of
    "--indent-size":rest -> case rest of
        [] -> Left "Missing value for indent size"
        indentSizeStr:rest' -> case readMaybe indentSizeStr of
          Nothing -> Left $ "Invalid indent size: " ++ indentSizeStr
          Just indentSize ->
            parse Config { configIndentSize = indentSize, .. } rest'

    "--max-width":rest -> case rest of
      [] -> Left "Missing value for max width"
      maxWidthStr:rest' -> case readMaybe maxWidthStr of
        Nothing -> Left $ "Invalid max width: " ++ maxWidthStr
        Just maxWidth ->
          parse Config { configMaxWidth = maxWidth, .. } rest'

    badArg:_ ->
      Left $ "Unknown argument: " ++ badArg

    [] -> Right Config{..}

data State = State
  { stateResult :: String
  , stateRest :: String
  , stateDepth :: Int
  , stateLineLength :: Int
  }

initialState input = State
  { stateResult = ""
  , stateRest = input
  , stateDepth = 0
  , stateLineLength = 0
  }

pprintParens :: Config -> String -> String
pprintParens Config{..} s =
  runState $ initialState s
  where
  indent depth = replicate (depth * configIndentSize) ' '

  -- Retuns Nothing if an open paren is closer.
  findCloseParenIndex :: String -> Maybe Int
  findCloseParenIndex s = case (elemIndex '(' s, elemIndex ')' s) of
    (Just open, Just close) -> if close < open then Just close else Nothing
    (_, mbClose) -> mbClose

  runState :: State -> String
  runState State{..} = case stateRest of
    '(':rest -> case findCloseParenIndex rest of
      Just index | index < configMaxWidth - stateLineLength - 1 ->
        let (consumed, newRest) = splitAt (index + 1) rest
        in runState State
            { stateResult = stateResult ++ "(" ++ consumed
            , stateRest = newRest
            , stateLineLength = stateLineLength + length consumed + 1
            , ..
            }
      _ ->
        let newDepth = stateDepth + 1
        in runState State
            { stateResult = stateResult ++ "(\n" ++ indent newDepth
            , stateRest = rest
            , stateDepth = newDepth
            , stateLineLength = length (indent newDepth)
            }
    ')':rest ->
      let newDepth = stateDepth - 1
      in runState State
        { stateResult = stateResult ++ "\n" ++ indent newDepth ++ ")"
        , stateRest = rest
        , stateDepth = newDepth
        , stateLineLength = length (indent newDepth) + 1
        }
    c:rest -> runState State
        { stateResult = stateResult ++ [c]
        , stateRest = rest
        , stateDepth = stateDepth
        , stateLineLength = stateLineLength + 1
        }
    "" -> stateResult
