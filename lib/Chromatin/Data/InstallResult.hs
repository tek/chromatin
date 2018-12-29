module Chromatin.Data.InstallResult(
  InstallResult(..),
) where

import Chromatin.Data.Rplugin (Rplugin)
import Chromatin.Data.RebuildTask (RebuildTask)

data InstallResult =
  Success Rplugin
  |
  Failure RebuildTask
  |
  PreviousFailure String RebuildTask
  deriving Show
