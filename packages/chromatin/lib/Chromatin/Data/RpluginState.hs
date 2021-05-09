module Chromatin.Data.RpluginState(
  RpluginState(..),
) where

data RpluginState =
  Broken Text
  |
  Ready
  |
  Incomplete
  deriving (Eq, Show)
