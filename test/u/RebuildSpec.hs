{-# OPTIONS_GHC -F -pgmF htfpp #-}

module RebuildSpec(
  htf_thisModulesTests
) where

import Data.Default.Class (Default(def))
import Control.Monad.IO.Class (liftIO)
import Test.Framework
import Neovim (vim_command')
import Ribosome.Config.Setting (updateSetting)
import Chromatin.Data.Chromatin (Chromatin)
import Chromatin.Data.Rplugins (Rplugins(Rplugins))
import Chromatin.Data.RpluginName (RpluginName(RpluginName))
import Chromatin.Data.RpluginConfig (RpluginConfig(RpluginConfig))
import Chromatin.Rebuild (crmRebuild)
import Chromatin.Test.Unit (specWithDef)
import qualified Chromatin.Settings as S (rplugins)
import Config (vars)

rplugins :: Rplugins
rplugins = Rplugins [
  RpluginConfig "hackage:proteome-0.1.0.0" (Just (RpluginName "proteome")),
  RpluginConfig "hackage:flagellum" Nothing
  ]

rebuildSpec :: Chromatin ()
rebuildSpec = do
  updateSetting S.rplugins rplugins
  crmRebuild def
  vim_command' "sleep 1"
  liftIO $ assertEqual [""] [""]

test_rebuild :: IO ()
test_rebuild =
  vars >>= specWithDef rebuildSpec
