module Chromatin.Rebuild(
  crmRebuild,
) where

import Conduit (ConduitT, (.|), runConduit, mapMC, sinkList)
import Control.Concurrent.STM.TBMChan (TBMChan, newTBMChan, writeTBMChan, closeTBMChan)
import qualified Control.Lens as Lens (set)
import Data.Conduit.TMChan (sourceTBMChan)
import Data.Foldable (traverse_)
import UnliftIO (atomically)
import UnliftIO.STM (TVar, STM)
import Neovim (CommandArguments)
import Ribosome.Data.Ribo (Ribo)
import qualified Ribosome.Data.Ribo as Ribo (inspect, modify)
import Ribosome.Internal.IO (forkNeovim)
import Chromatin.Data.Chromatin (Chromatin)
import Chromatin.Data.Env (Env, InstallTask(..))
import qualified Chromatin.Data.Env as Env (installerChan, _installerChan)
import Chromatin.Data.Rplugin (Rplugin)
import Chromatin.Data.RpluginName (RpluginName)
import Chromatin.Data.RpluginSource (RpluginSource)
import Chromatin.Config (readConfig, analyzeConfigIO, RpluginModification(RpluginNew))
import Chromatin.Install (installRplugin)
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

handleInstallTask :: InstallTask -> Chromatin (Either String Rplugin)
handleInstallTask Stop = stopInstaller >> return (Left "done")
handleInstallTask task = installRplugin task

handleInstallTaskC :: ConduitT InstallTask (Either String Rplugin) (Ribo (TVar Env)) ()
handleInstallTaskC = mapMC handleInstallTask

installerC :: ConduitT InstallTask (Either String Rplugin) (Ribo (TVar Env)) ()
installerC = handleInstallTaskC

createChan :: STM (TBMChan InstallTask)
createChan = newTBMChan 64

runInstaller :: TBMChan InstallTask -> Chromatin ()
runInstaller chan = do
  r <- runConduit $ sourceTBMChan chan .| installerC .| sinkList
  Log.debug $ "installer terminated: " ++ show r

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
