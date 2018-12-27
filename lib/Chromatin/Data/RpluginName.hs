{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Chromatin.Data.RpluginName(
  RpluginName(..),
) where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Neovim.Classes (NvimObject(..))
import Ribosome.Internal.NvimObject (deriveString)

newtype RpluginName = RpluginName String
  deriving (Eq, Show, Generic, NFData)

instance NvimObject RpluginName where
  toObject (RpluginName s) = toObject s
  fromObject = deriveString RpluginName
