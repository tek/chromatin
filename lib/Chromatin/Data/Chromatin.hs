module Chromatin.Data.Chromatin where

import Ribosome.Control.Monad.Ribo (ConcNvimS, Ribo, RiboE)
import Ribosome.Orphans ()

import Chromatin.Data.Env (Env)
import Chromatin.Data.Error (Error)

type EnvN = ConcNvimS Env
type Chromatin a = Ribo Env EnvN a
type ChromatinE e m a = RiboE Env e m a
type ChromatinN = RiboE Env Error EnvN
