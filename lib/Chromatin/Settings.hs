module Chromatin.Settings(
  rplugins,
) where

import Chromatin.Data.RpluginConfig (RpluginConfig)
import Data.Default.Class (Default(def))
import Ribosome.Config.Setting

rplugins :: Setting [RpluginConfig]
rplugins = Setting "rplugins" True (Just def)
