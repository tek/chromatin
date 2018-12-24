module Chromatin.Data.Chromatin(
  Chromatin,
) where

import UnliftIO.STM (TVar)
import Ribosome.Data.Ribo (Ribo)
import Chromatin.Data.Env (Env)

type Chromatin a = Ribo (TVar Env) a
