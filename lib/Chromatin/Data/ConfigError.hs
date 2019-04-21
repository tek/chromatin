{-# LANGUAGE TemplateHaskell #-}

module Chromatin.Data.ConfigError where

import Ribosome.Data.ErrorReport (ErrorReport(..))
import Ribosome.Error.Report.Class (ReportError(..))
import System.Log (Priority(ERROR))
import Text.ParserCombinators.Parsec (ParseError)

data ConfigError =
  InvalidPath Text Text
  |
  UnknownPrefix Text Text
  |
  ParseFailure Text ParseError
  deriving (Eq, Show)

deepPrisms ''ConfigError

instance ReportError ConfigError where
  errorReport (InvalidPath path err) =
    ErrorReport msg logMsg ERROR
    where
      msg = "invalid project path in config: " <> path
      logMsg = ["ConfigError.InvalidPath:", path, err]
  errorReport (UnknownPrefix spec prefix) =
    ErrorReport msg logMsg ERROR
    where
      msg = "unknown prefix `" <> prefix <> "` in spec `" <> spec <> "`"
      logMsg = ["ConfigError.UnknownPrefix `" <> prefix <> "` in " <> spec]
  errorReport (ParseFailure spec err) =
    ErrorReport msg logMsg ERROR
    where
      msg = "invalid rplugin spec: " <> spec
      logMsg = ["ConfigError.ParseFailure for " <> spec <> ":", show err]
