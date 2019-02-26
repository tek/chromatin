module Chromatin.Data.Chromatin(
  Chromatin,
  ChromatinE,
) where

import Chromatin.Data.Env (Env)
import Ribosome.Control.Monad.RiboE (RiboE)
import Ribosome.Control.Ribo (Ribo)
import UnliftIO.STM (TVar)

type Chromatin a = Ribo (TVar Env) a
type ChromatinE a = RiboE Env a
