{-# OPTIONS_GHC -F -pgmF htfpp #-}

module PypiSpec(
  htf_thisModulesTests
) where

import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Ribosome.Test.Embed (TestConfig(tcTimeout))
import Test.Framework

import Chromatin.Data.Chromatin (Chromatin)
import Chromatin.Data.Rplugin (Rplugin(Rplugin))
import Chromatin.Data.RpluginName (RpluginName(RpluginName))
import Chromatin.Data.RpluginSource (PypiDepspec(..), RpluginSource(Pypi))
import Chromatin.Rebuild.Build (installRplugin)
import qualified Chromatin.Rebuild.Build as InstallResult (InstallResult(Success))
import Chromatin.Test.Config (defaultTestConfig)
import Chromatin.Test.Unit (specConfig)

pypiSpec :: Chromatin ()
pypiSpec = do
  let name = RpluginName "myo"
  let source = Pypi (PypiDepspec "myo~=1.0.0a")
  result <- installRplugin name source
  liftIO $ assertEqual (InstallResult.Success (Rplugin name source)) result

testConfig :: TestConfig
testConfig = defaultTestConfig { tcTimeout = 30 }

test_pypi :: IO ()
test_pypi =
  specConfig testConfig def pypiSpec
