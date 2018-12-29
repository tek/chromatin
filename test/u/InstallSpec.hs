{-# OPTIONS_GHC -F -pgmF htfpp #-}

module InstallSpec(
  htf_thisModulesTests
) where

import Control.Monad.IO.Class (liftIO)
import Test.Framework
import UnliftIO.Directory (getCurrentDirectory)
import Chromatin.Data.Chromatin (Chromatin)
import Chromatin.Data.Rplugin (Rplugin(Rplugin))
import Chromatin.Data.RpluginName (RpluginName(RpluginName))
import Chromatin.Data.RpluginSource (RpluginSource(Stack))
import Chromatin.Test.Unit (specWithDef)
import Chromatin.Install (installRplugin)
import qualified Chromatin.Install as InstallResult (InstallResult(Success))
import Config (vars)

installSpec :: Chromatin ()
installSpec = do
  cwd <- getCurrentDirectory
  let name = RpluginName "chromatin"
  let source = Stack cwd
  result <- installRplugin name source
  liftIO $ assertEqual (InstallResult.Success (Rplugin name source)) result

test_install :: IO ()
test_install =
  vars >>= specWithDef installSpec
