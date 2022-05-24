#!/usr/bin/env stack
{- stack
  --resolver lts-18.27
  --install-ghc runghc
  --package bytestring
  --package http-client
  --package http-client-tls
  --package optparse-generic
  --package scalpel
  --package text
-}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wall #-}

import Control.Applicative
import Data.Foldable
import Text.HTML.Scalpel

import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as Lazy.Char8
import qualified Data.Text.Lazy as Lazy.Text
import qualified Data.Text.Lazy.Encoding as Lazy.Text
import qualified Data.Text.Lazy.IO as Lazy.Text
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Options.Applicative as Opts

main :: IO ()
main = do
  Args { word, internalHtml } <- parseArgs
  body <- case internalHtml of
    Just filePath -> do
      Lazy.Char8.readFile filePath
    Nothing -> do
      manager <- TLS.getGlobalManager
      request <- HTTP.parseRequest $ "https://www.merriam-webster.com/thesaurus/" <> word
      response <- HTTP.httpLbs request manager
      pure $ HTTP.responseBody response
  case scraper body of
    Nothing -> error "Failed to scrape!"
    Just ss ->
      for_ (Lazy.Text.split (== ',') $ Lazy.Text.decodeUtf8 ss) \s -> do
        Lazy.Text.putStrLn $ Lazy.Text.strip s

scraper :: Lazy.ByteString -> Maybe Lazy.ByteString
scraper body = scrapeStringLike body do
  text $ "ul" @: ["class" @= "mw-list"]

data Args = Args
  { word :: String
  , internalHtml :: Maybe FilePath
  } deriving stock (Show)

parseArgs :: IO Args
parseArgs = do
  Opts.execParser $ Opts.info (parser <**> Opts.helper) mempty
  where
  parser = do
    word <-
      Opts.strArgument
        (Opts.metavar "word")
    internalHtml <-
      Opts.option
        (Opts.maybeReader (Just . Just))
        (Opts.long "internal-html"
          <> Opts.value Nothing
          <> Opts.internal
        )
    pure Args { word, internalHtml }
