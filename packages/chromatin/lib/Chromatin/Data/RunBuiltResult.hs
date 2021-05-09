module Chromatin.Data.RunBuiltResult(
  RunBuiltResult(..),
) where

import Chromatin.Data.ActiveRplugin (ActiveRplugin)
import Chromatin.Data.RebuildTask (RebuildTask)

data RunBuiltResult =
  Success ActiveRplugin
  |
  Failure {
    task :: RebuildTask,
    errors :: [Text]
    }
  |
  PreviousFailure {
    stage :: Text,
    task :: RebuildTask,
    errors :: [Text]
  }
  deriving Show
