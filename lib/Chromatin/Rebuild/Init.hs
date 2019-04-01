module Chromatin.Rebuild.Init(
  initializeRplugins,
) where

import Control.Monad (when)
import Data.Default.Class (def)
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Maybe (catMaybes)
import Neovim (NeovimException, vim_call_function)
import Ribosome.Api.Exists (waitForFunction)
import qualified Ribosome.Api.Exists as Exists (function)
import Ribosome.Error.Report (reportError)

import Chromatin.Data.ActiveRplugin (ActiveRplugin(ActiveRplugin))
import Chromatin.Data.Chromatin (Chromatin)
import Chromatin.Data.Rplugin (Rplugin(Rplugin))
import Chromatin.Data.RpluginName (RpluginName(RpluginName))
import Chromatin.Data.String (capitalize)

activeRpluginName :: ActiveRplugin -> String
activeRpluginName (ActiveRplugin _ (Rplugin (RpluginName rpluginName) _)) = rpluginName

pluginRpcName :: String -> ActiveRplugin -> String
pluginRpcName rpcName rplugin =
  capitalize (activeRpluginName rplugin) ++ rpcName

waitForPlugin :: ActiveRplugin -> Chromatin (Maybe ActiveRplugin)
waitForPlugin rplugin = do
  result <- waitForFunction (pluginRpcName "Poll" rplugin) def
  case result of
    Right _ -> return $ Just rplugin
    Left _ -> do
      reportError "rebuild-init" $ "poll function of `" ++ activeRpluginName rplugin ++ "` did not appear"
      return Nothing

stageError :: ActiveRplugin -> Int -> NeovimException -> Chromatin ()
stageError rplugin stage err =
  reportError "rebuild-init" $ "error in stage " ++ show stage ++ " of `" ++ name ++ "`" ++ show err
  where
    name = activeRpluginName rplugin

runStage :: ActiveRplugin -> Int -> String -> Chromatin ()
runStage rplugin stage func = do
  result <- void <$> vim_call_function func []
  either (stageError rplugin stage) return result

runStageInRpluginIfExists :: Int -> ActiveRplugin -> Chromatin ()
runStageInRpluginIfExists stage rplugin = do
  exists <- Exists.function func
  when exists $ runStage rplugin stage func
  where
    func = pluginRpcName ("Stage" ++ show stage) rplugin

runStageInRplugins :: [ActiveRplugin] -> Int -> Chromatin ()
runStageInRplugins rplugins stage =
  traverse_ (runStageInRpluginIfExists stage) rplugins

runStages :: [ActiveRplugin] -> Chromatin ()
runStages rplugins =
  traverse_ (runStageInRplugins rplugins) [1..5]

initializeRplugins :: [ActiveRplugin] -> Chromatin ()
initializeRplugins rplugins = do
  results <- traverse waitForPlugin rplugins
  let crmPlugins = catMaybes results
  runStages crmPlugins
