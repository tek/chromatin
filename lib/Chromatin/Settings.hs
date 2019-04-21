module Chromatin.Settings(
  rplugins,
) where

import Chromatin.Data.RpluginConfig (RpluginConfig)
import Data.Default (Default(def))
import Ribosome.Data.Setting (Setting(Setting))

rplugins :: Setting [RpluginConfig]
rplugins = Setting "rplugins" True (Just def)
