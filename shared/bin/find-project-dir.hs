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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Prelude hiding (log)
import Control.Monad
import Data.Char (toLower, toUpper, isAlphaNum)
import Data.Foldable
import Data.List (elemIndex, isPrefixOf, nub, sort)
import Data.Maybe
import Data.IORef
import qualified Data.Text as T
import Data.Traversable
import qualified Data.Set as Set
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process
import System.IO
import System.IO.Unsafe (unsafePerformIO)

usage :: String
usage = "Usage: find-project-dir.hs <pattern> [--notify] [--complete] [--show-config]"

-- | Supply the FPD_DEBUG_LOG env var add debug logging.
{-# NOINLINE debug #-}
debug :: Bool
debug = unsafePerformIO $ isJust . lookup "FPD_DEBUG_LOG" <$> getEnvironment

main :: IO ()
main = getArgs >>= \case
  ["--show-config"] -> print _CONFIG
  ["--print-project-vars"] -> printProjectVars
  [pat, "--notify"] -> runWithArgs pat True
  [pat, "--complete"] -> runComplete pat
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

  runComplete :: String -> IO ()
  runComplete pat = do
    let Config {..} = _CONFIG
    aliasCompletions <- fmap concat $
      for configAliases $ \(alias, _) ->
        pure $ if pat `isPrefixOf` alias then [alias] else []
    rootCompletions <- fmap (concat . concat) $
      for configRoots $ \path -> do
        files <- listDirectoryIfExists path
        for files $ \file ->
          if file `elem` [".", ".."] then pure [] else do
            let fullPath = path </> file
            ifM (doesDirectoryExist fullPath)
              (pure [file])
              (pure [])
    let completions = nub $ aliasCompletions ++ rootCompletions
    forM_ completions putStrLn

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

{-# NOINLINE _CONFIG #-}
_CONFIG :: Config
_CONFIG = unsafePerformIO readConfig

readConfig :: IO Config
readConfig = do
  let path = homeDir </> ".find-project-dir"
  ifM (doesFileExist path)
    (parse parseInit mempty . lines <$> readFile path)
    (pure mempty)
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
  let Config {..} = _CONFIG
  case lookup pat configAliases of
    Just found -> do
      pure $ [[Just $ homeDir </> found]]
    Nothing -> search pat configRoots

-- | Returns an empty list if the directory does not exist.
-- Also, removes hidden files (those that start with dot '.')
listDirectoryIfExists :: FilePath -> IO [FilePath]
listDirectoryIfExists path =
  ifM (doesDirectoryExist path)
    (filter (not . isPrefixOf ".") <$> listDirectory path)
    (pure [])

search :: Traversable t => String -> t FilePath -> IO (t [Maybe FilePath])
search pat dirs = do
  for dirs $ \path -> do
    log $ "Checking path: " <> path
    files <- listDirectoryIfExists path
    for (sort files) $ \file -> do
      log $ "Checking: " <> file
      let fullPath = path </> file
      -- Only match directories.
      ifM (doesDirectoryExist fullPath)
        (if file `matches` pat then
          pure $ Just fullPath
        else
          pure Nothing)
        (pure Nothing)

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

printProjectVars :: IO ()
printProjectVars = do
  rootsAndProjects <- getRootsAndProjects
  seen <- newIORef Set.empty
  for_ rootsAndProjects $ \(root, projects) -> do
      for_ projects $ \project -> do
        let k = map toUpper $ filter isAlphaNum project
        inserted <- atomicModifyIORef' seen $ \s ->
          if Set.member k s then
            (s, False)
          else
            (Set.insert k s, True)
        when inserted $ putStrLn $
          "export PROJ_" <> k <> "=" <> root <> "/" <> project
  where
  Config {..} = _CONFIG

  getRootsAndProjects :: IO [(FilePath, [FilePath])]
  getRootsAndProjects = traverse (\r -> (r,) <$> listDirectoryIfExists r) configRoots

log :: String -> IO ()
log msg = when debug $ hPutStrLn stderr msg

-- | Like @if@, but where the test can be monadic.
-- Copy-pasta from 'extra:Control.Monad.Extra' to avoid the dep.
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mb t f = do b <- mb; if b then t else f
