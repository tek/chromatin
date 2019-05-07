{-# OPTIONS_GHC -F -pgmF htfpp #-}

module RebuildSpec(
  htf_thisModulesTests
) where

import Ribosome.Config.Setting (updateSetting)
import Ribosome.System.Time (sleep)
import Test.Framework

import Chromatin.Data.Chromatin (Chromatin)
import Chromatin.Data.RpluginConfig (RpluginConfig(RpluginConfig))
import Chromatin.Data.RpluginName (RpluginName(RpluginName))
import Chromatin.Rebuild (crmRebuild)
import qualified Chromatin.Settings as S (rplugins)
import Chromatin.Test.Unit (specWithDef)
import Config (vars)
import Test ()

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
  gassertBool True

test_rebuild :: IO ()
test_rebuild =
  vars >>= specWithDef rebuildSpec
