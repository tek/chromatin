{-# OPTIONS_GHC -F -pgmF htfpp #-}

module RebuildSpec(
  htf_thisModulesTests
) where

import Data.Default.Class (Default(def))
import Neovim (vim_command')
import Ribosome.Config.Setting (updateSettingR)
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
  RpluginConfig "hackage:proteome-0.1.0.0" (Just (RpluginName "proteome")) Nothing,
  RpluginConfig "hackage:flagellum" Nothing Nothing
  ]

rebuildSpec :: Chromatin ()
rebuildSpec = do
  updateSettingR S.rplugins rplugins
  crmRebuild def
  vim_command' "sleep 1"
  gassertBool True

test_rebuild :: IO ()
test_rebuild =
  vars >>= specWithDef rebuildSpec
