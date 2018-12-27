module Chromatin.Unload(
  unloadRplugins,
) where

import Data.Foldable (traverse_)
import qualified Control.Lens as Lens (over)
import qualified Ribosome.Data.Ribo as Ribo (modify)
import Chromatin.Data.Rplugin (Rplugin)
import Chromatin.Data.Chromatin (Chromatin)
import qualified Chromatin.Data.Env as Env (_rplugins)

unloadRplugin :: Rplugin -> Chromatin ()
unloadRplugin rplugin =
  Ribo.modify $ Lens.over Env._rplugins $ filter (rplugin ==)

unloadRplugins :: [Rplugin] -> Chromatin ()
unloadRplugins =
  traverse_ unloadRplugin
