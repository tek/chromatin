{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Chromatin.Data.RpluginConfig(
  RpluginConfig(..),
) where

import GHC.Generics (Generic)
import qualified Data.Map as Map (fromList)
import Data.Text.Prettyprint.Doc ((<+>), viaShow)
import Control.DeepSeq (NFData)
import Neovim (NvimObject(..), Dictionary, Object(ObjectMap))
import Ribosome.Internal.NvimObject (extractObject)
import Chromatin.Data.RpluginName (RpluginName)

data RpluginConfig =
  RpluginConfig {
    rpluginConfigSpec :: String,
    rpluginConfigName :: Maybe RpluginName
  }
  deriving (Show, Generic, NFData)

instance NvimObject RpluginConfig where
  toObject RpluginConfig {..} =
    (toObject :: Dictionary -> Object) . Map.fromList $
    [
      ("spec", toObject rpluginConfigSpec),
      ("name", toObject rpluginConfigName)
    ]
  fromObject (ObjectMap o) = do
    spec' <- extractObject "spec" o
    name' <- extractObject "name" o
    return $ RpluginConfig spec' name'
  fromObject o = Left ("invalid type for RpluginConfig: " <+> viaShow o)
