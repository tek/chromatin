{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Chromatin.Plugin(
  plugin,
)
where

import UnliftIO.STM (TVar)
import Neovim (
  Plugin(..),
  command',
  Neovim,
  StartupConfig,
  NeovimConfig,
  NeovimPlugin,
  wrapPlugin,
  )
import Ribosome.Control.Ribosome (Ribosome)
import Chromatin.Init (initialize)
import Chromatin.Data.Env (Env)
import Chromatin.Diag (crmDiag)
import Chromatin.Rebuild (crmRebuild)

plugin' :: Ribosome (TVar Env) -> Plugin (Ribosome (TVar Env))
plugin' env =
  Plugin {
    environment = env,
    exports = [
      $(command' 'crmDiag) [],
      $(command' 'crmRebuild) []
    ]
  }

plugin :: Neovim (StartupConfig NeovimConfig) NeovimPlugin
plugin = do
  env <- initialize
  wrapPlugin $ plugin' env
