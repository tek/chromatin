module Chromatin.Data.InstallResult(
  InstallResult(..),
) where

import Chromatin.Data.RebuildTask (RebuildTask)
import Chromatin.Data.Rplugin (Rplugin)

data InstallResult =
  Success Rplugin
  |
  Failure RebuildTask
  |
  PreviousFailure Text RebuildTask
  deriving Show
