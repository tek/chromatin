module Chromatin.Data.Rplugins(
  Rplugins(..),
) where

import Chromatin.Data.RpluginConfig (RpluginConfig)
import Data.Default.Class (Default)
import GHC.Generics (Generic)

newtype Rplugins =
  Rplugins [RpluginConfig]
  deriving (Eq, Show, Generic, Default)
