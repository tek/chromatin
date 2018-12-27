{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ConfigSpec(
  htf_thisModulesTests
) where

import Test.Framework
import Chromatin.Data.Rplugins (Rplugins(Rplugins))
import Chromatin.Data.RpluginName (RpluginName(RpluginName))
import Chromatin.Data.RpluginConfig (RpluginConfig(RpluginConfig))
import Chromatin.Data.RpluginSource (RpluginSource(Hackage), HackageDepspec(HackageDepspec))
import Chromatin.Config (analyzeConfig, RpluginModification(RpluginNew))

rplugins :: Rplugins
rplugins = Rplugins [
  RpluginConfig "hackage:proteome-0.1.0.0" (Just (RpluginName "proteome")),
  RpluginConfig "hackage:flagellum" Nothing
  ]

target :: Either String [RpluginModification]
target = Right [
  RpluginNew (RpluginName "proteome") (Hackage (HackageDepspec "proteome-0.1.0.0")),
  RpluginNew (RpluginName "flagellum") (Hackage (HackageDepspec "flagellum"))
  ]

test_analyzeConfig :: IO ()
test_analyzeConfig =
  assertEqual target (analyzeConfig [] rplugins)
