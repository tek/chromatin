{-# LANGUAGE TemplateHaskell #-}

module Chromatin.Data.Env(
  InstallTask(..),
  Env(..),
  _rplugins,
  _errors,
  _installerChan,
) where

import Control.Concurrent.STM.TBMChan (TBMChan)
import Control.Lens (makeClassy_)
import Data.Default.Class (Default(def))
import Ribosome.Data.Errors (Errors)
import Chromatin.Data.Rplugin (Rplugin)
import Chromatin.Data.RpluginName (RpluginName)
import Chromatin.Data.RpluginSource (RpluginSource)


data InstallTask =
  Install RpluginName RpluginSource
  |
  Stop
  deriving Show

data Env =
  Env {
    rplugins :: [Rplugin],
    errors :: Errors,
    installerChan :: Maybe (TBMChan InstallTask)
  }

makeClassy_ ''Env

instance Default Env where
  def = Env def def def
