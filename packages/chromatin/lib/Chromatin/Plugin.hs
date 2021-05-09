module Chromatin.Plugin where

import Neovim (Neovim, NeovimPlugin, Plugin(..), wrapPlugin)
import Ribosome.Control.Ribosome (Ribosome)
import Ribosome.Error.Report (reportError)
import Ribosome.Plugin (RpcDef, cmd, riboPlugin, rpcHandler)

import Chromatin.Data.Chromatin (Chromatin)
import Chromatin.Data.Env (Env)
import Chromatin.Data.Error (Error)
import Chromatin.Diag (crmDiag)
import Chromatin.Init (initialize)
import Chromatin.Rebuild (crmRebuild)

handleError :: Error -> Chromatin ()
handleError =
  reportError "chromatin"

rpcHandlers :: [[RpcDef (Ribo Env Error)]]
rpcHandlers =
  [
    $(rpcHandler (cmd []) 'crmDiag),
    $(rpcHandler (cmd []) 'crmRebuild)
    ]

plugin' :: Ribosome Env -> Plugin (Ribosome Env)
plugin' env =
  riboPlugin "chromatin" env rpcHandlers def handleError def

plugin :: Neovim e NeovimPlugin
plugin =
  wrapPlugin . plugin' =<< initialize
