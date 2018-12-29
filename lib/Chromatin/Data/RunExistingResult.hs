module Chromatin.Data.RunExistingResult(
  RunExistingResult(..),
) where

import Chromatin.Data.ActiveRplugin (ActiveRplugin)
import Chromatin.Data.RebuildTask (RebuildTask)

data RunExistingResult =
  Success ActiveRplugin
  |
  Failure RebuildTask [String]
  |
  NotReady RebuildTask
  deriving Show
