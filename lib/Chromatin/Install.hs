module Chromatin.Install(
  installRplugin,
) where

import Chromatin.Data.Rplugin (Rplugin)
import Chromatin.Data.Chromatin (Chromatin)
import Chromatin.Data.Env (InstallTask(..))

installRplugin :: InstallTask -> Chromatin (Either String Rplugin)
installRplugin _ = return $ Left "NI"
