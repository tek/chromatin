module Chromatin.Rebuild where

import Conduit (ConduitT, await, mapMC, runConduit, sinkList, yield, (.|))
import Control.Concurrent.Lifted (fork)
import Control.Concurrent.STM.TBMChan (TBMChan, closeTBMChan, newTBMChan, writeTBMChan)
import Control.Monad.Catch (MonadMask, MonadThrow)
import Data.Conduit.TMChan (sourceTBMChan)
import Ribosome.Data.PersistError (PersistError)
import Ribosome.Data.ScratchOptions (defaultScratchOptions)
import Ribosome.Data.SettingError (SettingError)
import qualified Ribosome.Log as Log (debug)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Scratch (showInScratch)

import Chromatin.Config (RpluginModification(RpluginNew), analyzeConfigIO, readConfig)
import Chromatin.Data.ActiveRplugin (ActiveRplugin)
import Chromatin.Data.ConfigError (ConfigError)
import Chromatin.Data.Env (Env)
import qualified Chromatin.Data.Env as Env (installerChan)
import Chromatin.Data.RebuildControl (RebuildControl)
import qualified Chromatin.Data.RebuildControl as RebuildControl (RebuildControl(..))
import Chromatin.Data.RebuildTask (RebuildTask(..))
import Chromatin.Data.RpluginName (RpluginName(RpluginName))
import Chromatin.Data.RpluginSource (RpluginSource)
import Chromatin.Data.RunBuiltResult (RunBuiltResult)
import qualified Chromatin.Data.RunBuiltResult as RunBuiltResult (RunBuiltResult(..))
import Chromatin.Data.RunExistingResult (RunExistingResult)
import Chromatin.Rebuild.Existing (handleExisting)
import Chromatin.Rebuild.Init (initializeRplugins)
import Chromatin.Rebuild.Nonexisting (handleNonexisting)

mapMChan ::
  MonadDeepState s Env m =>
  m a ->
  (TBMChan RebuildControl -> m a) ->
  m a
mapMChan fEmpty f = do
  storedChan <- getL @Env Env.installerChan
  maybe fEmpty f storedChan

stopRebuilder ::
  MonadRibo m =>
  MonadDeepState s Env m =>
  m ()
stopRebuilder =
  mapMChan (return ()) $ \c -> do
    setL @Env Env.installerChan Nothing
    atomically $ closeTBMChan c
    Log.debug ("stopped installer conduit" :: Text)

createChan :: STM (TBMChan RebuildControl)
createChan = newTBMChan 64

extractFailure :: RunBuiltResult -> [Text]
extractFailure (RunBuiltResult.Failure (RebuildTask (RpluginName name) _ _ _) err) =
  ("error when runnning plugin `" <> name <> "`:") : err
extractFailure (RunBuiltResult.Success _) = []
extractFailure (RunBuiltResult.PreviousFailure stage (RebuildTask (RpluginName name) _ _ _) err) =
  ("error in stage `" <> stage <> "` for plugin `" <> name <> "`:") : err

extractSuccess :: RunBuiltResult -> [ActiveRplugin]
extractSuccess (RunBuiltResult.Success arp) = [arp]
extractSuccess _ = []

controlC ::
  MonadRibo m =>
  MonadDeepState s Env m =>
  ConduitT RebuildControl RebuildTask m ()
controlC = do
  ctrl <- await
  case ctrl of
    Just RebuildControl.Stop -> lift stopRebuilder
    Just (RebuildControl.Continue task) -> yield task >> controlC
    Nothing -> controlC

handleExistingC ::
  NvimE e m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  ConduitT RebuildTask RunExistingResult m ()
handleExistingC = mapMC handleExisting

handleNonexistingC ::
  MonadRibo m =>
  NvimE e m =>
  MonadMask m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  ConduitT RunExistingResult RunBuiltResult m ()
handleNonexistingC = mapMC handleNonexisting

rebuilder ::
  MonadRibo m =>
  NvimE e m =>
  MonadMask m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  MonadDeepState s Env m =>
  TBMChan RebuildControl ->
  ConduitT () Void m [RunBuiltResult]
rebuilder chan = sourceTBMChan chan
  .| controlC
  .| handleExistingC
  .| handleNonexistingC
  .| sinkList

runRebuilder ::
  MonadRibo m =>
  NvimE e m =>
  MonadMask m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepState s Env m =>
  TBMChan RebuildControl ->
  m ()
runRebuilder chan = do
  result <- runConduit $ rebuilder chan
  case concatMap extractFailure result of
    err@(_:_) -> void $ showInScratch err (defaultScratchOptions "chromatin-rebuild-error")
    _ -> return ()
  initializeRplugins (concatMap extractSuccess result)
  Log.debug $ ("installer terminated: " <> show result :: Text)

forkRebuilder ::
  MonadRibo m =>
  NvimE e m =>
  MonadMask m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepState s Env m =>
  m (TBMChan RebuildControl)
forkRebuilder = do
  chan <- atomically createChan
  setL @Env Env.installerChan (Just chan)
  void $ fork $ runRebuilder chan
  return chan

enqueue ::
  MonadIO m =>
  RebuildControl ->
  TBMChan RebuildControl ->
  m ()
enqueue ctrl chan = atomically $ writeTBMChan chan ctrl

executeRpluginNew ::
  MonadRibo m =>
  NvimE e m =>
  MonadMask m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepState s Env m =>
  RpluginName ->
  RpluginSource ->
  Bool ->
  Bool ->
  m ()
executeRpluginNew name source dev debug = do
  chan <- mapMChan forkRebuilder return
  enqueue (RebuildControl.Continue (RebuildTask name source dev debug)) chan

executeModification ::
  MonadRibo m =>
  NvimE e m =>
  MonadMask m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepState s Env m =>
  RpluginModification ->
  m ()
executeModification (RpluginNew name source dev debug) =
  executeRpluginNew name source dev debug
executeModification _ =
  return ()

executeModifications ::
  MonadRibo m =>
  NvimE e m =>
  MonadMask m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepState s Env m =>
  [RpluginModification] ->
  m ()
executeModifications mods = do
  traverse_ executeModification mods
  mapMChan (return ()) (enqueue RebuildControl.Stop)

crmRebuild ::
  MonadRibo m =>
  NvimE e m =>
  MonadMask m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e ConfigError m =>
  MonadDeepState s Env m =>
  m ()
crmRebuild = do
  config <- readConfig
  mods <- analyzeConfigIO config
  executeModifications mods
