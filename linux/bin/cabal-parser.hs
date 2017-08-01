#!/usr/bin/env stack
-- stack --resolver lts-5.9 --install-ghc runghc --package pretty-show

import Control.Monad
import Distribution.PackageDescription.Parse
import System.IO
import Text.Show.Pretty

main :: IO ()
main = do
  contents <- getContents
  case parsePackageDescription contents of
    ParseFailed msg ->
      hPutStrLn stderr $ show msg

    ParseOk warnings result -> do
      mapM (hPutStrLn stderr . show) warnings
      putStrLn $ ppShow result

