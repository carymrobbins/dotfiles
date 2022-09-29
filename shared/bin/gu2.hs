#!/usr/bin/env stack
{- stack
  --resolver lts-18.27
  --install-ghc runghc
  --package aeson
  --package yaml
-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wall #-}

import Data.Aeson (FromJSON)
import Data.Foldable (for_)
import Data.Yaml (decodeFileThrow)
import GHC.Generics (Generic)
import System.Environment (getArgs)
import System.Process (callProcess)

main :: IO ()
main = do
  config <- readConfig
  forEachVersion config \ghcVersion cabalVersion -> do
    let runArgs =
          [ "run"
          , "--ghc", ghcVersion
          , "--cabal", cabalVersion
          , "--quick"
          , "--"
          , "cabal", "test", "all"
          , "--builddir=" <> "gubuild/ghc-" <> ghcVersion <> "-cabal-" <> cabalVersion
          ]
    putStrLn $ "+ ghcup " <> unwords runArgs
    callProcess "ghcup" runArgs

readConfig :: IO Config
readConfig = decodeFileThrow ".ghcup.yaml"

parseArgs :: IO [String]
parseArgs = getArgs >>= \case

data Config = Config
  { matrix :: MatrixConfig
  } deriving stock (Generic, Show)
    deriving anyclass (FromJSON)

data MatrixConfig = MatrixConfig
  { ghc :: [GHCVersion]
  , cabal :: [CabalVersion]
  } deriving stock (Generic, Show)
    deriving anyclass (FromJSON)

type GHCVersion = String
type CabalVersion = String

forEachVersion :: Config -> (GHCVersion -> CabalVersion -> IO ()) -> IO ()
forEachVersion Config { matrix = MatrixConfig { ghc, cabal } } f =
  for_ ((,) <$> ghc <*> cabal) \(ghcVersion, cabalVersion) -> do
    f ghcVersion cabalVersion
