module Chromatin.Data.RpluginSource where

import Path (Abs, Dir, Path)

newtype HackageDepspec =
  HackageDepspec Text
  deriving (Show, Eq)

newtype PypiDepspec =
  PypiDepspec Text
  deriving (Show, Eq)

data RpluginSource =
  Hackage {
    hackageSpec :: HackageDepspec
  }
  |
  Stack {
    stackProject :: Path Abs Dir
  }
  |
  Pypi {
    pypiSpec :: PypiDepspec
  }
  deriving (Show, Eq)
