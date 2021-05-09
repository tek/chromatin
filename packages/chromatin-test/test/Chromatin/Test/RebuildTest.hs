module Chromatin.Test.RebuildTest where

import Ribosome.Config.Setting (updateSetting)
import Ribosome.System.Time (sleep)
import Ribosome.Test.Run (UnitTest)

import Chromatin.Data.Chromatin (Chromatin)
import Chromatin.Data.RpluginConfig (RpluginConfig(RpluginConfig))
import Chromatin.Data.RpluginName (RpluginName(RpluginName))
import Chromatin.Rebuild (crmRebuild)
import qualified Chromatin.Settings as S (rplugins)
import Chromatin.Test.Config (vars)
import Chromatin.Test.Unit (specWithDef)

rplugins :: [RpluginConfig]
rplugins = [
  RpluginConfig "hackage:proteome-0.1.0.0" (Just (RpluginName "proteome")) Nothing Nothing,
  RpluginConfig "hackage:flagellum" Nothing Nothing Nothing
  ]

rebuildSpec :: Chromatin ()
rebuildSpec = do
  updateSetting S.rplugins rplugins
  crmRebuild
  sleep 1

test_rebuild :: UnitTest
test_rebuild =
  vars >>= specWithDef (lift rebuildSpec)
