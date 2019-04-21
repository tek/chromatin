{-# LANGUAGE TemplateHaskell #-}

module Chromatin.Data.Env where

import Chromatin.Data.RebuildControl (RebuildControl)
import Chromatin.Data.Rplugin (Rplugin)
import Control.Concurrent.STM.TBMChan (TBMChan)
import Data.Default (Default(def))
import Ribosome.Data.Errors (Errors)

data Env =
  Env {
    _rplugins :: [Rplugin],
    _errors :: Errors,
    _installerChan :: Maybe (TBMChan RebuildControl)
  }

deepLenses ''Env

instance Default Env where
  def = Env def def def
