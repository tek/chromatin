module Chromatin.Test.RunTest where

import Hedgehog ((===))
import Ribosome.Msgpack.Encode (toMsgpack)
import Ribosome.Nvim.Api.IO (vimCallFunction)
import Ribosome.System.Time (sleep)
import Ribosome.Test.Await (await)
import Ribosome.Test.Run (UnitTest)
import Ribosome.Test.Tmux (tmuxGuiSpecDef)

import Chromatin.Data.ActiveRplugin (ActiveRplugin(ActiveRplugin))
import Chromatin.Data.Rplugin (Rplugin(Rplugin))
import Chromatin.Data.RpluginName (RpluginName(RpluginName))
import Chromatin.Data.RpluginSource (RpluginSource(Stack))
import Chromatin.Run (runRplugin)
import qualified Chromatin.Run as RunRpluginResult (RunRpluginResult(Success))
import Chromatin.Test.Unit (ChromatinTest)
import Ribosome.Test.Unit (fixture)
import Path (parseAbsDir)
import qualified Chromatin.Rebuild.Build as InstallResult
import Chromatin.Rebuild.Build (installRplugin)

runSpec :: ChromatinTest ()
runSpec = do
  dir <- parseAbsDir =<< fixture "stack-project"
  let
    source =
      Stack dir
    rplugin =
      Rplugin name source
    name =
      RpluginName "crm-test"
  (InstallResult.Success rplugin ===) =<< lift (installRplugin name source)
  result <- runRplugin rplugin False
  RunRpluginResult.Success (ActiveRplugin 4 rplugin) === result
  sleep 0.5
  await ((toMsgpack (2 :: Int)) ===) $ vimCallFunction "exists" [toMsgpack (":CrmTest" :: ByteString)]

test_run :: UnitTest
test_run =
  tmuxGuiSpecDef runSpec
