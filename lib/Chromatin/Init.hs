module Chromatin.Init(
  initialize,
) where

import Data.Default.Class (Default(def))
import System.Log.Logger (updateGlobalLogger, setLevel, Priority(ERROR))
import Control.Monad.IO.Class (liftIO)
import UnliftIO.STM (TVar)
import Neovim (Neovim)
import Neovim.Context.Internal (Config(customConfig), asks')
import Ribosome.Data.Ribosome (newRibosome, Ribosome)
import Ribosome.Internal.IO (retypeNeovim)
import Chromatin.Data.Chromatin (Chromatin)
import Chromatin.Data.Env (Env)

initialize' :: Chromatin (Ribosome (TVar Env))
initialize' =
  asks' customConfig

initialize :: Neovim e (Ribosome (TVar Env))
initialize = do
  liftIO $ updateGlobalLogger "Neovim.Plugin" (setLevel ERROR)
  ribo <- newRibosome "chromatin" def
  retypeNeovim (const ribo) initialize'
