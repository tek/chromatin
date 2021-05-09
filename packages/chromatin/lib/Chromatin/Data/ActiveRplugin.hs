module Chromatin.Data.ActiveRplugin(
  ActiveRplugin(..),
) where

import Chromatin.Data.Rplugin (Rplugin)

data ActiveRplugin =
  ActiveRplugin {
    arChannelId :: Int,
    arRplugin :: Rplugin
  }
  deriving (Show, Eq)
