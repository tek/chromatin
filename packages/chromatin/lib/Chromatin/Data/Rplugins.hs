module Chromatin.Data.Rplugins where

import Chromatin.Data.RpluginConfig (RpluginConfig)

newtype Rplugins =
  Rplugins [RpluginConfig]
  deriving (Eq, Show, Generic)
  deriving newtype (Default)
