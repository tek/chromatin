module Chromatin.Init where

import Control.Monad.IO.Class (liftIO)
import Data.Default (Default(def))
import Neovim (Neovim)
import Neovim.Context.Internal (Config(customConfig), asks')
import Ribosome.Control.Monad.Ribo (riboE2ribo, runRib)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Error.Report (reportError')
import Ribosome.Internal.IO (retypeNeovim)
import System.Log.Logger (Priority(ERROR), setLevel, updateGlobalLogger)

import Chromatin.Data.Chromatin (Chromatin)
import Chromatin.Data.Env (Env)
import Chromatin.Data.Error (Error)
import Chromatin.Rebuild (crmRebuild)

initialize' :: Chromatin (Ribosome Env)
initialize' = do
  result <- riboE2ribo @Error crmRebuild
  reportError' "init" result
  lift (asks' customConfig)

initialize :: Neovim e (Ribosome Env)
initialize = do
  liftIO $ updateGlobalLogger "Neovim.Plugin" (setLevel ERROR)
  ribo <- newRibosome "chromatin" def
  retypeNeovim (const ribo) (runRib initialize')
