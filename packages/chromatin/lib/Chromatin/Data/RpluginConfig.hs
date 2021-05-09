module Chromatin.Data.RpluginConfig where

import Ribosome.Msgpack.Decode (MsgpackDecode)
import Ribosome.Msgpack.Encode (MsgpackEncode)

import Chromatin.Data.RpluginName (RpluginName)

data RpluginConfig =
  RpluginConfig {
    spec :: Text,
    name :: Maybe RpluginName,
    dev :: Maybe Bool,
    debug :: Maybe Bool
  }
  deriving (Eq, Show, Generic, MsgpackDecode, MsgpackEncode)
