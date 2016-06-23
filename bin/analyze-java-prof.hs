#!/usr/bin/env runhaskell

{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import qualified Data.Map.Lazy as Map
import Data.List
import Data.Ord
import System.Environment
import System.Exit
import System.IO

main = getArgs >>= handleArgs

handleArgs args = case args of
  [] -> getContents >>= run

  ["--help"] -> getHelpText >>= putStrLn
  ["-h"] -> getHelpText >>= putStrLn

  [flag@('-':_)] -> do
    hPutStrLn stderr $ "Unknown flag: " ++ flag
    hPutStrLn stderr ""
    getHelpText >>= hPutStrLn stderr
    exitFailure

  [file] -> readFile file >>= run

  bad -> do
    hPutStrLn stderr $ "Unsupported arguments: " ++ unwords bad
    hPutStrLn stderr ""
    getHelpText >>= hPutStrLn stderr
    exitFailure

  where
  getHelpText :: IO String
  getHelpText = do
    progName <- getProgName
    return $ unlines
      [ "Usage: " ++ progName ++ " [java.prof]"
      , "       " ++ progName ++ " < java.prof"
      , ""
      , "Generate a java.prof file by simply using the '-prof' flag, e.g. -"
      , ""
      , "  % java -prof Main"
      , ""
      , "You can then analyze the top 10 methods with the longest execution"
      , "time via the following -"
      , ""
      , "  % " ++ progName ++ " java.prof | head -n11 | column -t"
      , ""
      , "You can even filter for the specific class patterns you are"
      , "looking for with 'grep'"
      , ""
      , "  % " ++ progName ++ " java.prof | grep '^Main' | column -t"
      , ""
      , "The fields are tab-delimited, so you can also export to a file and"
      , "analyze it using your favorite spreadsheet editor"
      , ""
      , "  % " ++ progName ++ " java.prof > main-profile.tsv"
      ]

run contents = do
  let contentLines = lines contents
  case contentLines of
    [] -> error "Empty prof file"

    header:rows -> do
      assert (header == "count callee caller time") "Invalid header"
      let parsed = map parseRow rows
      printCols ["Callee", "Total"]
      forM_ (calleeTimes parsed) $ \(callee, totalTime) ->
        printCols [callee, show totalTime]

printCols :: [String] -> IO ()
printCols = putStrLn . intercalate "\t"

calleeTimes :: [Row] -> [(String, Int)]
calleeTimes rows =
  reverse $ sortBy (comparing snd) . Map.toList $ foldr f Map.empty rows
  where
  f Row{..} m = Map.insertWith (+) rowCallee (rowCount * rowTime) m

assert t msg = unless t $ error msg

parseRow :: String -> Row
parseRow row = Row{..}
  where
  (rowCount, ' ':rest) = mapFst parseInt $ break (== ' ') row
  (rowCallee, ' ':rest') = break (== ' ') rest
  (rowCaller, ' ':rest'') =
    if isPrefixOf unknownCaller rest' then
      splitAt (length unknownCaller) rest'
    else break (== ' ') rest'
  rowTime = parseInt rest''
  unknownCaller = "<unknown caller>"

parseInt :: String -> Int
parseInt s = read s

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

data Row = Row
  { rowCount :: Int
  , rowCallee :: String
  , rowCaller :: String
  , rowTime :: Int
  } deriving (Show)
