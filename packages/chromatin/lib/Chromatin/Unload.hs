module Chromatin.Unload where

import Chromatin.Data.Chromatin (Chromatin)
import Chromatin.Data.Env (Env)
import qualified Chromatin.Data.Env as Env (rplugins)
import Chromatin.Data.Rplugin (Rplugin)

unloadRplugin :: Rplugin -> Chromatin ()
unloadRplugin rplugin =
  modifyL @Env Env.rplugins $ filter (rplugin ==)

unloadRplugins :: [Rplugin] -> Chromatin ()
unloadRplugins =
  traverse_ unloadRplugin
