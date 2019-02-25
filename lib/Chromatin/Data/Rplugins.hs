module Chromatin.Data.Rplugins(
  Rplugins(..),
) where

import Chromatin.Data.RpluginConfig (RpluginConfig)
import Control.DeepSeq (NFData)
import Data.Default.Class (Default)
import GHC.Generics (Generic)
import Neovim.Classes (NvimObject(..))

newtype Rplugins =
  Rplugins [RpluginConfig]
  deriving (Generic, NFData, Default)

instance NvimObject Rplugins where
  toObject (Rplugins cs) = toObject cs
  fromObject o = fmap Rplugins (fromObject o)
