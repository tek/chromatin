{-# OPTIONS_GHC -F -pgmF htfpp #-}

module RunSpec(
  htf_thisModulesTests
) where

import Control.Monad.IO.Class (liftIO)
import Test.Framework
import UnliftIO.Directory (getCurrentDirectory)
import Chromatin.Data.Chromatin (Chromatin)
import Chromatin.Data.ActiveRplugin (ActiveRplugin(ActiveRplugin))
import Chromatin.Data.Rplugin (Rplugin(Rplugin))
import Chromatin.Data.RpluginName (RpluginName(RpluginName))
import Chromatin.Data.RpluginSource (RpluginSource(Stack))
import Chromatin.Test.Unit (specWithDef)
import Chromatin.Run (runRplugin)
import qualified Chromatin.Run as RunResult (RunResult(Success))
import Config (vars)

installSpec :: Chromatin ()
installSpec = do
  cwd <- getCurrentDirectory
  let rplugin = Rplugin (RpluginName "chromatin") (Stack cwd)
  result <- runRplugin rplugin
  liftIO $ assertEqual (RunResult.Success (ActiveRplugin 3 rplugin)) result

test_run :: IO ()
test_run =
  vars >>= specWithDef installSpec
