module Chromatin.Data.RpluginSource(
  HackageDepspec(..),
  PypiDepspec(..),
  RpluginSource(..),
) where

newtype HackageDepspec =
  HackageDepspec String
  deriving (Show, Eq)

newtype PypiDepspec =
  PypiDepspec String
  deriving (Show, Eq)

data RpluginSource =
  Hackage {
    hackageSpec :: HackageDepspec
  }
  |
  Stack {
    stackProject :: FilePath
  }
  |
  Pypi {
    pypiSpec :: PypiDepspec
  }
  deriving (Show, Eq)
