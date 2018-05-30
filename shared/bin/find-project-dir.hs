#!/usr/bin/env runhaskell

-- Compile with: make-find-project-dir

{- You'll want a ~/.find-project-dir file with something like the following -

roots:
$HOME/build
$HOME/projects
$HOME/work

aliases:
dot=dotfiles
hf=projects/intellij-haskforce
-}

{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (log)
import Control.Monad
import Data.Char (toLower)
import Data.List (elemIndex, sort)
import Data.Maybe
import qualified Data.Text as T
import Data.Traversable
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process
import System.IO
import System.IO.Unsafe (unsafePerformIO)

usage :: String
usage = "Usage: find-project-dir.hs <pattern> [--notify]"

-- | Supply the FPD_DEBUG_LOG env var add debug logging.
{-# NOINLINE debug #-}
debug :: Bool
debug = unsafePerformIO $ isJust . lookup "FPD_DEBUG_LOG" <$> getEnvironment

main :: IO ()
main = getArgs >>= \case
  ["--show-config"] -> readConfig >>= print
  [pat, "--notify"] -> runWithArgs pat True
  [pat] -> runWithArgs pat False
  _ -> hPutStrLn stderr usage
  where
  runWithArgs :: String -> Bool -> IO ()
  runWithArgs pat notify = do
    results <- run pat
    case catMaybes . join $ results of
      [] -> do
        let errMsg = "No matching project for '" ++ pat ++ "'"
        hPutStrLn stderr $ errMsg
        when notify $ void $ rawSystem "notify-send" [errMsg]
        exitFailure

      -- Print the first match
      x:_ -> putStrLn x

data Config = Config
  { configRoots :: [String]
  , configAliases :: [(String, String)]
  }
  deriving Show

mkConfigRoot :: String -> Config
mkConfigRoot r = Config [r] mempty

mkConfigAlias :: (String, String) -> Config
mkConfigAlias kv = Config mempty [kv]

instance Semigroup Config where
  Config rs1 as1 <> Config rs2 as2 = Config (rs1 <> rs2) (as1 <> as2)

instance Monoid Config where
  mempty = Config mempty mempty

{-# NOINLINE homeDir #-}
homeDir :: String
homeDir = unsafePerformIO getHomeDirectory

readConfig :: IO Config
readConfig = do
  let path = homeDir </> ".find-project-dir"
  exists <- doesFileExist path
  if exists then
    parse parseInit mempty . lines <$> readFile path
  else
    return mempty
  where
  parse p config fileLines = case fileLines of
    [] -> config
    x:xs -> case x of
      "" -> parse p config xs
      "roots:" -> parse parseRoot config xs
      "aliases:" -> parse parseAlias config xs
      _ -> parse p (config <> p x) xs

  parseInit x = error $ "Expected 'roots:' or 'aliases:'; got: " <> show x

  parseRoot x =
    mkConfigRoot $ T.unpack $ T.replace "$HOME" (T.pack homeDir) (T.pack x)

  parseAlias x = case T.splitOn "=" $ T.pack x of
    [k, v] -> mkConfigAlias (T.unpack k, T.unpack v)
    _ -> error $ "Invalid alias: " <> show x

run :: String -> IO [[Maybe String]]
run pat = do
  Config {..} <- readConfig
  case lookup pat configAliases of
    Just found -> do
      return $ [[Just $ homeDir </> found]]
    Nothing -> search pat configRoots

search :: Traversable t => String -> t FilePath -> IO (t [Maybe FilePath])
search pat dirs = do
  for dirs $ \dir -> do
    log $ "Checking dir: " <> dir
    let path = homeDir </> dir
    files <- getDirectoryContents path
    for (sort files) $ \file ->
      if file `elem` [".", ".."] then return Nothing else do
        log $ "Checking: " <> file
        let fullPath = path </> file
        isDir <- doesDirectoryExist $ fullPath
        if not isDir then
          return Nothing
        else if file `matches` pat then
          return $ Just fullPath
        else
          return Nothing

matches :: String -> String -> Bool
file `matches` pat
  | fileLower == patLower = True
  | otherwise =
      case (fileLower, patLower) of
        (x:xs, y:ys) | x == y -> loop xs ys
        (_,    _)             -> False
  where
  patLower = map toLower pat
  fileLower = map toLower file

  loop _ [] = True
  loop [] _ = False
  loop restFile (c:restPat) = case c `elemIndex` restFile of
    Nothing -> False
    Just n -> loop (drop (n + 1) restFile) restPat

log :: String -> IO ()
log msg = when debug $ hPutStrLn stderr msg
