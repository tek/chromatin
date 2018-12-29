{-# LANGUAGE TemplateHaskell #-}

module Chromatin.Data.Env(
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
import Chromatin.Data.RebuildControl (RebuildControl)

data Env =
  Env {
    rplugins :: [Rplugin],
    errors :: Errors,
    installerChan :: Maybe (TBMChan RebuildControl)
  }

makeClassy_ ''Env

instance Default Env where
  def = Env def def def
