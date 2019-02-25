module Chromatin.Data.RpluginName(
  RpluginName(..),
) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Neovim.Classes (NvimObject(..))
import Ribosome.Internal.NvimObject (deriveString)

newtype RpluginName = RpluginName String
  deriving (Eq, Show, Generic, NFData)

instance NvimObject RpluginName where
  toObject (RpluginName s) = toObject s
  fromObject = deriveString RpluginName
