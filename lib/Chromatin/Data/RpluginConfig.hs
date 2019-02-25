{-# LANGUAGE DeriveAnyClass #-}

module Chromatin.Data.RpluginConfig(
  RpluginConfig(..),
) where

import Control.DeepSeq (NFData)
import qualified Data.Map as Map (fromList)
import Data.Text.Prettyprint.Doc ((<+>), viaShow)
import GHC.Generics (Generic)
import Neovim (NvimObject(..), Dictionary, Object(ObjectMap))
import Ribosome.Internal.NvimObject (extractObject)

import Chromatin.Data.RpluginName (RpluginName)

data RpluginConfig =
  RpluginConfig {
    rpluginConfigSpec :: String,
    rpluginConfigName :: Maybe RpluginName,
    rpluginConfigDev :: Maybe Bool
  }
  deriving (Show, Generic, NFData)

instance NvimObject RpluginConfig where
  toObject RpluginConfig {..} =
    (toObject :: Dictionary -> Object) . Map.fromList $
    [
      ("spec", toObject rpluginConfigSpec),
      ("name", toObject rpluginConfigName),
      ("dev", toObject rpluginConfigDev)
    ]
  fromObject (ObjectMap o) = do
    spec' <- extractObject "spec" o
    name' <- extractObject "name" o
    dev' <- extractObject "dev" o
    return $ RpluginConfig spec' name' dev'
  fromObject o = Left ("invalid type for RpluginConfig: " <+> viaShow o)
