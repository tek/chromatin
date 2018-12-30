module Chromatin.Rebuild.Init(
  initializeRplugins,
) where

import Control.Monad (when)
import Data.Default.Class (def)
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Maybe (catMaybes)
import Data.Strings (strCapitalize)
import Neovim (NeovimException, vim_call_function)
import Ribosome.Api.Exists (waitForFunction)
import qualified Ribosome.Api.Exists as Exists (function)
import Chromatin.Data.ActiveRplugin (ActiveRplugin(ActiveRplugin))
import Chromatin.Data.Chromatin (Chromatin)
import Ribosome.Data.Errors (ComponentName(ComponentName))
import Chromatin.Data.Rplugin (Rplugin(Rplugin))
import Chromatin.Data.RpluginName (RpluginName(RpluginName))
import Chromatin.Env (logError)

activeRpluginName :: ActiveRplugin -> String
activeRpluginName (ActiveRplugin _ (Rplugin (RpluginName rpluginName) _)) = rpluginName

pluginRpcName :: String -> ActiveRplugin -> String
pluginRpcName rpcName rplugin =
  strCapitalize (activeRpluginName rplugin) ++ rpcName

waitForPlugin :: ActiveRplugin -> Chromatin (Maybe ActiveRplugin)
waitForPlugin rplugin = do
  result <- waitForFunction (pluginRpcName "Poll" rplugin) def
  return $ case result of
    Right _ -> Just rplugin
    Left _ -> Nothing

stageError :: ActiveRplugin -> Int -> NeovimException -> Chromatin ()
stageError rplugin stage err =
  logError (ComponentName "rebuild-init") $ "error in stage " ++ show stage ++ " of `" ++ name ++ "`" ++ show err
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
