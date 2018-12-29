module Chromatin.Rebuild(
  crmRebuild,
) where

import Conduit (ConduitT, (.|), runConduit, mapMC, sinkList)
import Control.Concurrent.STM.TBMChan (TBMChan, newTBMChan, writeTBMChan, closeTBMChan)
import qualified Control.Lens as Lens (set)
import Data.Conduit.TMChan (sourceTBMChan)
import Data.Foldable (traverse_)
import Data.Functor (void)
import UnliftIO (atomically)
import UnliftIO.STM (TVar, STM)
import Neovim (CommandArguments)
import Ribosome.Control.Ribo (Ribo)
import qualified Ribosome.Control.Ribo as Ribo (inspect, modify)
import Ribosome.Data.ScratchOptions (defaultScratchOptions)
import Ribosome.Internal.IO (forkNeovim)
import Ribosome.Scratch (showInScratch)
import Chromatin.Data.Chromatin (Chromatin)
import Chromatin.Data.Env (Env, InstallTask(..))
import qualified Chromatin.Data.Env as Env (installerChan, _installerChan)
import Chromatin.Data.RpluginName (RpluginName)
import Chromatin.Data.RpluginSource (RpluginSource)
import Chromatin.Config (readConfig, analyzeConfigIO, RpluginModification(RpluginNew))
import Chromatin.Install (installRplugin, InstallResult)
import qualified Chromatin.Install as InstallResult (InstallResult(..))
import Chromatin.Run (runRplugin, RunResult(..))
import qualified Chromatin.Run as RunResult (RunResult(..))
import qualified Chromatin.Log as Log

mapMChan :: Chromatin a -> (TBMChan InstallTask -> Chromatin a) -> Chromatin a
mapMChan fEmpty f = do
  storedChan <- Ribo.inspect Env.installerChan
  maybe fEmpty f storedChan

stopInstaller :: Chromatin ()
stopInstaller =
  mapMChan (return ()) $ \c -> do
    Ribo.modify $ Lens.set Env._installerChan Nothing
    atomically $ closeTBMChan c
    Log.debug "stopped installer conduit"

handleInstallTask :: InstallTask -> Chromatin InstallResult
handleInstallTask Stop = stopInstaller >> return (InstallResult.Failure [])
handleInstallTask (Install name source) = installRplugin name source

installerC :: ConduitT InstallTask InstallResult (Ribo (TVar Env)) ()
installerC = mapMC handleInstallTask

runInstallResult :: InstallResult -> Chromatin RunResult
runInstallResult (InstallResult.Failure err) = return $ RunResult.Failure err
runInstallResult (InstallResult.Success rplugin) = runRplugin rplugin

runnerC :: ConduitT InstallResult RunResult (Ribo (TVar Env)) ()
runnerC = mapMC runInstallResult

createChan :: STM (TBMChan InstallTask)
createChan = newTBMChan 64

extractFailure :: RunResult -> [String]
extractFailure (RunResult.Failure err) = err
extractFailure (RunResult.Success _) = []

runInstaller :: TBMChan InstallTask -> Chromatin ()
runInstaller chan = do
  result <- runConduit $ sourceTBMChan chan .| installerC .| runnerC .| sinkList
  case concatMap extractFailure result of
    err@(_:_) -> void $ showInScratch err (defaultScratchOptions "chromatin-install-error")
    _ -> return ()
  Log.debug $ "installer terminated: " ++ show result

forkInstaller :: Chromatin (TBMChan InstallTask)
forkInstaller = do
  chan <- atomically createChan
  Ribo.modify $ Lens.set Env._installerChan (Just chan)
  forkNeovim $ runInstaller chan
  return chan

enqueueInstallTask :: InstallTask -> TBMChan InstallTask -> Chromatin ()
enqueueInstallTask task chan = atomically $ writeTBMChan chan task

executeRpluginNew :: RpluginName -> RpluginSource -> Chromatin ()
executeRpluginNew name source = do
  chan <- mapMChan forkInstaller return
  enqueueInstallTask (Install name source) chan

executeModification :: RpluginModification -> Chromatin ()
executeModification (RpluginNew name source) = executeRpluginNew name source
executeModification _ = return ()

executeModifications :: [RpluginModification] -> Chromatin ()
executeModifications mods = do
  traverse_ executeModification mods
  mapMChan (return ()) (enqueueInstallTask Stop)

analysisError :: String -> Chromatin ()
analysisError _ = return ()

crmRebuild :: CommandArguments -> Chromatin ()
crmRebuild _ = do
  config <- readConfig
  analysis <- analyzeConfigIO config
  case analysis of
    Right mods -> executeModifications mods
    Left e -> analysisError e

  -- installNew mod
  -- unloadRplugins removed
  -- setupPlugins new ++ changed
