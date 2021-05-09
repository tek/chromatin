module Chromatin.Test.NixTest where

import Hedgehog ((===))
import Ribosome.Msgpack.Encode (toMsgpack)
import Ribosome.Nvim.Api.IO (vimCallFunction)
import Ribosome.Test.Await (await)
import Ribosome.Test.Run (UnitTest)
import Ribosome.Test.Tmux (tmuxGuiSpecDef)

import Chromatin.Data.ActiveRplugin (ActiveRplugin(ActiveRplugin))
import Chromatin.Data.Rplugin (Rplugin(Rplugin))
import Chromatin.Data.RpluginName (RpluginName(RpluginName))
import Chromatin.Data.RpluginSource (RpluginSource(Flake))
import qualified Chromatin.Rebuild.Build as InstallResult
import Chromatin.Rebuild.Build (installRplugin)
import Chromatin.Run (runRplugin)
import qualified Chromatin.Run as RunRpluginResult (RunRpluginResult(Success))
import Chromatin.Test.Unit (ChromatinTest)

runSpec :: ChromatinTest ()
runSpec = do
  (InstallResult.Success rplugin ===) =<< lift (installRplugin name source)
  result <- runRplugin rplugin False
  RunRpluginResult.Success (ActiveRplugin 5 rplugin) === result
  await ((toMsgpack (2 :: Int)) ===) $ vimCallFunction "exists" [toMsgpack (":CrmTest" :: ByteString)]
  where
    rplugin =
      Rplugin name source
    name =
      RpluginName "crm-test"
    source =
      Flake url
    url =
      "github:tek/crm-test?ref=b2ebd6f86acfae5a9c6a5bc7aae9db8621ecb984"

test_nix :: UnitTest
test_nix =
  tmuxGuiSpecDef runSpec
