module Chromatin.Settings(
  rplugins,
) where

import Data.Default.Class (Default(def))
import Ribosome.Config.Setting
import Chromatin.Data.Rplugins (Rplugins)

rplugins :: Setting Rplugins
rplugins = Setting "rplugins" True (Just def)
