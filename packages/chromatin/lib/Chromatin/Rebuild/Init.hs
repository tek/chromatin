module Chromatin.Rebuild.Init where

import Ribosome.Api.Exists (Retry(Retry), waitForFunctionResult)
import qualified Ribosome.Api.Exists as Exists (function)
import Ribosome.Data.Text (capitalize)
import Ribosome.Error.Report (reportError)
import Ribosome.Nvim.Api.IO (vimCallFunction, vimCommand)

import Chromatin.Data.ActiveRplugin (ActiveRplugin(ActiveRplugin))
import Chromatin.Data.Rplugin (Rplugin(Rplugin))
import Chromatin.Data.RpluginName (RpluginName(RpluginName))

activeRpluginName :: ActiveRplugin -> Text
activeRpluginName (ActiveRplugin _ (Rplugin (RpluginName rpluginName) _)) = rpluginName

pluginRpcName :: Text -> ActiveRplugin -> Text
pluginRpcName rpcName rplugin =
  capitalize (activeRpluginName rplugin) <> rpcName

waitForPlugin ::
  MonadRibo m =>
  NvimE e m =>
  ActiveRplugin ->
  m (Maybe ActiveRplugin)
waitForPlugin rplugin = do
  result <- waitForFunctionResult (pluginRpcName "Poll" rplugin) True (Retry 6 0.1)
  case result of
    Right _ -> return $ Just rplugin
    Left _ -> do
      reportError "rebuild-init" $ "poll function of `" <> activeRpluginName rplugin <> "` did not appear"
      return Nothing

stageError ::
  MonadRibo m =>
  NvimE e m =>
  ActiveRplugin ->
  Int ->
  RpcError ->
  m ()
stageError rplugin stage err =
  reportError "rebuild-init" $ "error in stage " <> show stage <> " of `" <> name <> "`" <> show err
  where
    name = activeRpluginName rplugin

runStage ::
  MonadRibo m =>
  NvimE e m =>
  ActiveRplugin ->
  Int ->
  Text ->
  m ()
runStage rplugin stage func =
  catchAt (stageError rplugin stage) (vimCallFunction func [])

runStageInRpluginIfExists ::
  MonadRibo m =>
  NvimE e m =>
  Int ->
  ActiveRplugin ->
  m ()
runStageInRpluginIfExists stage rplugin = do
  exists <- Exists.function func
  when exists $ runStage rplugin stage func
  where
    func = pluginRpcName ("Stage" <> show stage) rplugin

runStageInRplugins ::
  MonadRibo m =>
  NvimE e m =>
  [ActiveRplugin] ->
  Int ->
  m ()
runStageInRplugins rplugins stage =
  traverse_ (runStageInRpluginIfExists stage) rplugins

runStages ::
  MonadRibo m =>
  NvimE e m =>
  [ActiveRplugin] ->
  m ()
runStages rplugins =
  traverse_ @[] (runStageInRplugins rplugins) [1..5]

initializeRplugins ::
  MonadRibo m =>
  NvimE e m =>
  [ActiveRplugin] ->
  m ()
initializeRplugins rplugins = do
  results <- traverse waitForPlugin rplugins
  let crmPlugins = catMaybes results
  runStages crmPlugins
  vimCommand "silent! doautocmd User ChromatinInitialized"
