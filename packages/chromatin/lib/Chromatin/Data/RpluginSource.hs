module Chromatin.Data.RpluginSource where

import Path (Abs, Dir, Path)

newtype HackageDepspec =
  HackageDepspec Text
  deriving (Show, Eq)

newtype FlakeUrl =
  FlakeUrl { unFlakeUrl :: Text }
  deriving (Eq, Show, Generic)
  deriving newtype (IsString)

data RpluginSource =
  Hackage { hackageSpec :: HackageDepspec }
  |
  Stack { stackProject :: Path Abs Dir }
  |
  Flake { url :: FlakeUrl }
  deriving (Show, Eq)
