module Chromatin.Rebuild(
  crmRebuild,
) where

import Conduit (ConduitT, (.|), runConduit, mapMC, sinkList, await, yield)
import Control.Concurrent.STM.TBMChan (TBMChan, newTBMChan, writeTBMChan, closeTBMChan)
import qualified Control.Lens as Lens (set)
import Control.Monad.Trans.Class (lift)
import Data.Conduit.TMChan (sourceTBMChan)
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Void (Void)
import UnliftIO (atomically)
import UnliftIO.STM (TVar, STM)
import Neovim (CommandArguments)
import Ribosome.Control.Ribo (Ribo)
import qualified Ribosome.Control.Ribo as Ribo (inspect, modify)
import Ribosome.Data.ScratchOptions (defaultScratchOptions)
import Ribosome.Internal.IO (forkNeovim)
import Ribosome.Scratch (showInScratch)
import Chromatin.Data.ActiveRplugin (ActiveRplugin)
import Chromatin.Data.Chromatin (Chromatin)
import Chromatin.Data.Env (Env)
import qualified Chromatin.Data.Env as Env (installerChan, _installerChan)
import Chromatin.Data.RebuildTask (RebuildTask(..))
import Chromatin.Data.RebuildControl (RebuildControl)
import qualified Chromatin.Data.RebuildControl as RebuildControl (RebuildControl(..))
import Chromatin.Data.RunExistingResult (RunExistingResult)
import Chromatin.Data.RunBuiltResult (RunBuiltResult)
import qualified Chromatin.Data.RunBuiltResult as RunBuiltResult (RunBuiltResult(..))
import Chromatin.Data.RpluginName (RpluginName(RpluginName))
import Chromatin.Data.RpluginSource (RpluginSource)
import Chromatin.Config (readConfig, analyzeConfigIO, RpluginModification(RpluginNew))
import Chromatin.Rebuild.Existing (handleExisting)
import Chromatin.Rebuild.Init (initializeRplugins)
import Chromatin.Rebuild.Nonexisting (handleNonexisting)
import qualified Chromatin.Log as Log

mapMChan :: Chromatin a -> (TBMChan RebuildControl -> Chromatin a) -> Chromatin a
mapMChan fEmpty f = do
  storedChan <- Ribo.inspect Env.installerChan
  maybe fEmpty f storedChan

stopRebuilder :: Chromatin ()
stopRebuilder =
  mapMChan (return ()) $ \c -> do
    Ribo.modify $ Lens.set Env._installerChan Nothing
    atomically $ closeTBMChan c
    Log.debug "stopped installer conduit"

createChan :: STM (TBMChan RebuildControl)
createChan = newTBMChan 64

extractFailure :: RunBuiltResult -> [String]
extractFailure (RunBuiltResult.Failure (RebuildTask (RpluginName name) _) err) =
  ("error when runnning plugin `" ++ name ++ "`:") : err
extractFailure (RunBuiltResult.Success _) = []
extractFailure (RunBuiltResult.PreviousFailure stage (RebuildTask (RpluginName name) _) err) =
  ("error in stage `" ++ stage ++ "` for plugin `" ++ name ++ "`:") : err

extractSuccess :: RunBuiltResult -> [ActiveRplugin]
extractSuccess (RunBuiltResult.Success arp) = [arp]
extractSuccess _ = []

controlC :: ConduitT RebuildControl RebuildTask (Ribo (TVar Env)) ()
controlC = do
  ctrl <- await
  case ctrl of
    Just RebuildControl.Stop -> lift stopRebuilder
    Just (RebuildControl.Continue task) -> yield task >> controlC
    Nothing -> controlC

handleExistingC :: ConduitT RebuildTask RunExistingResult (Ribo (TVar Env)) ()
handleExistingC = mapMC handleExisting

handleNonexistingC :: ConduitT RunExistingResult RunBuiltResult (Ribo (TVar Env)) ()
handleNonexistingC = mapMC handleNonexisting

rebuilder :: TBMChan RebuildControl -> ConduitT () Void (Ribo (TVar Env)) [RunBuiltResult]
rebuilder chan = sourceTBMChan chan
  .| controlC
  .| handleExistingC
  .| handleNonexistingC
  .| sinkList

runRebuilder :: TBMChan RebuildControl -> Chromatin ()
runRebuilder chan = do
  result <- runConduit $ rebuilder chan
  case concatMap extractFailure result of
    err@(_:_) -> void $ showInScratch err (defaultScratchOptions "chromatin-rebuild-error")
    _ -> return ()
  initializeRplugins (concatMap extractSuccess result)
  Log.debug $ "installer terminated: " ++ show result

forkRebuilder :: Chromatin (TBMChan RebuildControl)
forkRebuilder = do
  chan <- atomically createChan
  Ribo.modify $ Lens.set Env._installerChan (Just chan)
  forkNeovim $ runRebuilder chan
  return chan

enqueue :: RebuildControl -> TBMChan RebuildControl -> Chromatin ()
enqueue ctrl chan = atomically $ writeTBMChan chan ctrl

executeRpluginNew :: RpluginName -> RpluginSource -> Chromatin ()
executeRpluginNew name source = do
  chan <- mapMChan forkRebuilder return
  enqueue (RebuildControl.Continue (RebuildTask name source)) chan

executeModification :: RpluginModification -> Chromatin ()
executeModification (RpluginNew name source) = executeRpluginNew name source
executeModification _ = return ()

executeModifications :: [RpluginModification] -> Chromatin ()
executeModifications mods = do
  traverse_ executeModification mods
  mapMChan (return ()) (enqueue RebuildControl.Stop)

analysisError :: String -> Chromatin ()
analysisError _ = return ()

crmRebuild :: CommandArguments -> Chromatin ()
crmRebuild _ = do
  config <- readConfig
  analysis <- analyzeConfigIO config
  case analysis of
    Right mods -> executeModifications mods
    Left e -> analysisError e
