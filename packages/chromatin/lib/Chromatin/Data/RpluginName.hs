module Chromatin.Data.RpluginName where

import Neovim.Classes (NvimObject(..))
import Ribosome.Internal.NvimObject (deriveString)
import Ribosome.Msgpack.Decode (MsgpackDecode)
import Ribosome.Msgpack.Encode (MsgpackEncode)

newtype RpluginName =
  RpluginName Text
  deriving (Eq, Show, Generic)
  deriving newtype (NFData, MsgpackDecode, MsgpackEncode)

instance NvimObject RpluginName where
  toObject (RpluginName s) = toObject s
  fromObject = deriveString RpluginName
