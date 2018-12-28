{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Chromatin.Data.Rplugins(
  Rplugins(..),
) where

import GHC.Generics (Generic)
import Data.Default.Class (Default)
import Control.DeepSeq (NFData)
import Neovim.Classes (NvimObject(..))
import Chromatin.Data.RpluginConfig (RpluginConfig)

newtype Rplugins =
  Rplugins [RpluginConfig]
  deriving (Generic, NFData, Default)

instance NvimObject Rplugins where
  toObject (Rplugins cs) = toObject cs
  fromObject o = fmap Rplugins (fromObject o)