{-# OPTIONS_GHC -F -pgmF htfpp #-}

module BuildSpec(
  htf_thisModulesTests
) where

import Control.Monad.IO.Class (liftIO)
import Path.IO (getCurrentDir)
import Test.Framework

import Chromatin.Data.Chromatin (Chromatin)
import Chromatin.Data.Rplugin (Rplugin(Rplugin))
import Chromatin.Data.RpluginName (RpluginName(RpluginName))
import Chromatin.Data.RpluginSource (RpluginSource(Stack))
import Chromatin.Rebuild.Build (installRplugin)
import qualified Chromatin.Rebuild.Build as InstallResult (InstallResult(Success))
import Chromatin.Test.Unit (specWithDef)
import Config (vars)

stackSpec :: Chromatin ()
stackSpec = do
  cwd <- getCurrentDir
  let name = RpluginName "chromatin"
  let source = Stack cwd
  result <- installRplugin name source
  liftIO $ assertEqual (InstallResult.Success (Rplugin name source)) result

test_stack :: IO ()
test_stack =
  vars >>= specWithDef stackSpec
