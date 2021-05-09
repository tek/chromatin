module Chromatin.Test.BuildTest where

import Hedgehog ((===))
import Ribosome.Test.Run (UnitTest)

import Chromatin.Data.Rplugin (Rplugin(Rplugin))
import Chromatin.Data.RpluginName (RpluginName(RpluginName))
import Chromatin.Data.RpluginSource (RpluginSource(Stack))
import Chromatin.Rebuild.Build (installRplugin)
import qualified Chromatin.Rebuild.Build as InstallResult (InstallResult(Success))
import Chromatin.Test.Config (vars)
import Chromatin.Test.Unit (ChromatinTest, specWithDef)
import Ribosome.Test.Unit (fixture)
import Path (parseAbsDir)

stackSpec :: ChromatinTest ()
stackSpec = do
  dir <- parseAbsDir =<< fixture "stack-project"
  let name = RpluginName "stack-project"
  let source = Stack dir
  result <- lift $ installRplugin name source
  InstallResult.Success (Rplugin name source) === result

test_stack :: UnitTest
test_stack =
  vars >>= specWithDef stackSpec
