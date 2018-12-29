{-# OPTIONS_GHC -F -pgmF htfpp #-}

module InstallSpec(
  htf_thisModulesTests
) where

import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (def)
import Test.Framework
import UnliftIO.Directory (getCurrentDirectory)
import Ribosome.Test.Embed (TestConfig(tcTimeout))
import Chromatin.Data.Chromatin (Chromatin)
import Chromatin.Data.Rplugin (Rplugin(Rplugin))
import Chromatin.Data.RpluginName (RpluginName(RpluginName))
import Chromatin.Data.RpluginSource (RpluginSource(Stack, Pypi), PypiDepspec(..))
import Chromatin.Test.Unit (specWithDef, specConfig)
import Chromatin.Test.Config (defaultTestConfig)
import Chromatin.Install (installRplugin)
import qualified Chromatin.Install as InstallResult (InstallResult(Success))
import Config (vars)

stackSpec :: Chromatin ()
stackSpec = do
  cwd <- getCurrentDirectory
  let name = RpluginName "chromatin"
  let source = Stack cwd
  result <- installRplugin name source
  liftIO $ assertEqual (InstallResult.Success (Rplugin name source)) result

test_stack :: IO ()
test_stack =
  vars >>= specWithDef stackSpec

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
