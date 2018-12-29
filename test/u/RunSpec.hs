{-# OPTIONS_GHC -F -pgmF htfpp #-}

module RunSpec(
  htf_thisModulesTests
) where

import Control.Monad.IO.Class (liftIO)
import Neovim (vim_call_function', toObject, vim_command)
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
  _ <- vim_command "sleep 500m"
  exists <- vim_call_function' "exists" [toObject ":CrmDiag"]
  liftIO $ assertEqual (RunResult.Success (ActiveRplugin 3 rplugin)) result
  liftIO $ assertEqual (toObject (2 :: Int)) exists

test_run :: IO ()
test_run =
  vars >>= specWithDef installSpec
