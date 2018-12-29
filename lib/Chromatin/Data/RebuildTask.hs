module Chromatin.Data.RebuildTask(
  RebuildTask(..),
) where

import Chromatin.Data.RpluginName (RpluginName)
import Chromatin.Data.RpluginSource (RpluginSource)

data RebuildTask =
  RebuildTask RpluginName RpluginSource
  deriving Show
