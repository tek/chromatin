module Chromatin.Data.RebuildControl(
  RebuildControl(..),
) where

import Chromatin.Data.RebuildTask (RebuildTask)

data RebuildControl =
  Continue RebuildTask
  |
  Stop
  deriving Show
