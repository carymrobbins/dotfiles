#!/usr/bin/env runhaskell

{-# LANGUAGE LambdaCase #-}

import Control.Monad
import Data.Foldable
import Data.List (elemIndex)
import Data.Maybe
import Data.Traversable
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO

(=:) = (,)

aliases =
  [ "hf" =: "projects/intellij-haskforce" ]

usage = "Usage: find-project-dir.hs <pattern>"

-- Relative to $HOME
dirs = ["work", "projects", "build"]

main = getArgs >>= \case
  [pat] -> do
    results <- run pat
    case catMaybes . join $ results of
      [] -> do
        hPutStrLn stderr $ "No matching project for '" ++ pat ++ "'"
        exitFailure

      -- Print the first match
      x:_ -> putStrLn x

  _ -> hPutStrLn stderr usage

run pat = case lookup pat aliases of
  Just found -> do
    home <- getHomeDirectory
    return $ [[Just $ home </> found]]
  Nothing -> search pat

search pat = do
  home <- getHomeDirectory
  for dirs $ \dir -> do
    let path = home </> dir
    files <- getDirectoryContents path
    for files $ \file -> do
      let fullPath = path </> file
      isDir <- doesDirectoryExist $ fullPath
      if not isDir then
        return Nothing
      else if file `matches` pat then
        return $ Just fullPath
      else
        return Nothing

file `matches` pat
  | file == pat = True
  | otherwise = loop file pat
  where
  loop _ [] = True
  loop [] _ = False
  loop restFile (c:restPat) = case c `elemIndex` restFile of
    Nothing -> False
    Just n -> loop (drop (n + 1) restFile) restPat
