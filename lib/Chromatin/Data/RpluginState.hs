module Chromatin.Data.RpluginState(
  RpluginState(..),
) where

data RpluginState =
  Broken String
  |
  Ready
  |
  Incomplete
  deriving (Eq, Show)
