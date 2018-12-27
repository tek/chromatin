module Chromatin.Data.Rplugin(
  Rplugin(..),
) where

import Chromatin.Data.RpluginName (RpluginName)
import Chromatin.Data.RpluginSource (RpluginSource)

data Rplugin =
  Rplugin {
    name :: RpluginName,
    source :: RpluginSource
  }
  deriving (Show, Eq)
