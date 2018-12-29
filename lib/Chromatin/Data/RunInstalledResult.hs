module Chromatin.Data.RunInstalledResult(
  RunInstalledResult(..),
) where

import Chromatin.Data.ActiveRplugin (ActiveRplugin)
import Chromatin.Data.RebuildTask (RebuildTask)

data RunInstalledResult =
  Success ActiveRplugin
  |
  Failure {
    task :: RebuildTask,
    errors :: [String]
    }
  |
  PreviousFailure {
    stage :: String,
    task :: RebuildTask,
    errors :: [String]
  }
  deriving Show
