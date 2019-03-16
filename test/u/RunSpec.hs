{-# OPTIONS_GHC -F -pgmF htfpp #-}

module RunSpec(
  htf_thisModulesTests
) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Neovim (toObject, vim_call_function', vim_command)
import Test.Framework
import UnliftIO.Directory (getCurrentDirectory)

import Chromatin.Data.ActiveRplugin (ActiveRplugin(ActiveRplugin))
import Chromatin.Data.Chromatin (Chromatin)
import Chromatin.Data.Rplugin (Rplugin(Rplugin))
import Chromatin.Data.RpluginName (RpluginName(RpluginName))
import Chromatin.Data.RpluginSource (RpluginSource(Stack))
import Chromatin.Run (runRplugin)
import qualified Chromatin.Run as RunRpluginResult (RunRpluginResult(Success))
import Chromatin.Test.Unit (specWithDef)
import Config (vars)

runSpec :: Chromatin ()
runSpec = do
  cwd <- getCurrentDirectory
  let rplugin = Rplugin (RpluginName "chromatin") (Stack cwd)
  result <- runRplugin rplugin False
  _ <- vim_command "sleep 500m"
  exists <- vim_call_function' "exists" [toObject (":CrmDiag" :: ByteString)]
  liftIO $ assertEqual (RunRpluginResult.Success (ActiveRplugin 3 rplugin)) result
  liftIO $ assertEqual (toObject (2 :: Int)) exists

test_run :: IO ()
test_run =
  vars >>= specWithDef runSpec
