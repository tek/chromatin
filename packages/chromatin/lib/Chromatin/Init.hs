module Chromatin.Init where

import Neovim (Neovim)
import Neovim.Context.Internal (Config(customConfig), asks')
import Ribosome.Control.Monad.Ribo (RNeovim, runRibo)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Error.Report (reportError')
import Ribosome.Internal.IO (retypeNeovim)
import System.Log.Logger (Priority(ERROR), setLevel, updateGlobalLogger)

import Chromatin.Data.Env (Env)
import Chromatin.Data.Error (Error)
import Chromatin.Rebuild (crmRebuild)

initialize' :: RNeovim Env (Ribosome Env)
initialize' = do
  (result :: Either Error ()) <- runRibo crmRebuild
  reportError' "init" result
  asks' customConfig

initialize :: Neovim e (Ribosome Env)
initialize = do
  liftIO $ updateGlobalLogger "Neovim.Plugin" (setLevel ERROR)
  ribo <- newRibosome "chromatin" def
  retypeNeovim (const ribo) initialize'
