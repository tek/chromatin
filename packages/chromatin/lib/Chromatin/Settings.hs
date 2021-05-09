module Chromatin.Settings where

import Ribosome.Data.Setting (Setting(Setting))

import Chromatin.Data.RpluginConfig (RpluginConfig)

rplugins :: Setting [RpluginConfig]
rplugins = Setting "rplugins" True (Just def)
