module Chromatin.Test.ConfigTest where

import Hedgehog (TestT, evalEither, (===))

import Chromatin.Config (RpluginModification(RpluginNew, RpluginUpdate), analyzeConfig)
import Chromatin.Data.Rplugin (Rplugin(Rplugin))
import Chromatin.Data.RpluginConfig (RpluginConfig(RpluginConfig))
import Chromatin.Data.RpluginName (RpluginName(RpluginName))
import Chromatin.Data.RpluginSource (HackageDepspec(HackageDepspec), RpluginSource(Hackage))
import Chromatin.Data.Rplugins (Rplugins(Rplugins))

proteomeName :: RpluginName
proteomeName =
  RpluginName "proteome"

proteomeSrc :: RpluginSource
proteomeSrc =
  Hackage (HackageDepspec "proteome-0.1.0.0")

proteome :: Rplugin
proteome =
  Rplugin proteomeName proteomeSrc

rplugins :: Rplugins
rplugins = Rplugins [
  RpluginConfig "hackage:proteome-0.1.0.0" (Just proteomeName) Nothing Nothing,
  RpluginConfig "hackage:flagellum" Nothing Nothing Nothing
  ]

target :: [RpluginModification]
target = [
  RpluginUpdate proteome proteomeSrc False False,
  RpluginNew (RpluginName "flagellum") (Hackage (HackageDepspec "flagellum")) False False
  ]

test_analyzeConfig ::
  Monad m =>
  TestT m ()
test_analyzeConfig =
  (target ===) =<< evalEither (analyzeConfig [proteome] rplugins)
