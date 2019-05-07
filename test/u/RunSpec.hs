{-# OPTIONS_GHC -F -pgmF htfpp #-}

module RunSpec (htf_thisModulesTests) where

import Data.ByteString (ByteString)
import Path.IO (getCurrentDir)
import Ribosome.Msgpack.Encode (toMsgpack)
import Ribosome.Nvim.Api.IO (vimCallFunction)
import Ribosome.System.Time (sleep)
import Ribosome.Test.Await (await)
import Ribosome.Test.Tmux (tmuxGuiSpecDef)
import Test.Framework

import Chromatin.Data.ActiveRplugin (ActiveRplugin(ActiveRplugin))
import Chromatin.Data.Chromatin (Chromatin)
import Chromatin.Data.Rplugin (Rplugin(Rplugin))
import Chromatin.Data.RpluginName (RpluginName(RpluginName))
import Chromatin.Data.RpluginSource (RpluginSource(Stack))
import Chromatin.Run (runRplugin)
import qualified Chromatin.Run as RunRpluginResult (RunRpluginResult(Success))

runSpec :: Chromatin ()
runSpec = do
  cwd <- getCurrentDir
  let rplugin = Rplugin (RpluginName "chromatin") (Stack cwd)
  result <- runRplugin rplugin False
  gassertEqual (RunRpluginResult.Success (ActiveRplugin 4 rplugin)) result
  sleep 0.5
  await (gassertEqual (toMsgpack (2 :: Int))) $ vimCallFunction "exists" [toMsgpack (":CrmDiag" :: ByteString)]

test_run :: IO ()
test_run =
  tmuxGuiSpecDef runSpec
