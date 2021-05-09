module Chromatin.Rebuild.Nonexisting where

import Control.Monad.Catch (MonadMask)
import Ribosome.Data.PersistError (PersistError)
import Ribosome.Data.SettingError (SettingError)

import Chromatin.Data.RebuildTask (RebuildTask(RebuildTask))
import qualified Chromatin.Data.RebuildTask as RebuildTask (RebuildTask(debug))
import Chromatin.Data.Rplugin (Rplugin)
import Chromatin.Data.RunBuiltResult (RunBuiltResult)
import qualified Chromatin.Data.RunBuiltResult as RunBuiltResult (RunBuiltResult(..))
import Chromatin.Data.RunExistingResult (RunExistingResult)
import qualified Chromatin.Data.RunExistingResult as RunExistingResult (RunExistingResult(..))
import Chromatin.Rebuild.Build (InstallResult, installRplugin)
import qualified Chromatin.Rebuild.Build as InstallResult (InstallResult(Success, Failure))
import Chromatin.Run (RunRpluginResult, runRplugin)
import qualified Chromatin.Run as RunRpluginResult (RunRpluginResult(..))

runBuiltResult :: RebuildTask -> RunRpluginResult -> RunBuiltResult
runBuiltResult _ (RunRpluginResult.Success active) = RunBuiltResult.Success active
runBuiltResult task (RunRpluginResult.Failure err) = RunBuiltResult.Failure task err

installSuccess ::
  NvimE e m =>
  MonadBaseControl IO m =>
  MonadIO m =>
  RebuildTask ->
  Rplugin ->
  m RunBuiltResult
installSuccess task rplugin =
  runBuiltResult task <$> runRplugin rplugin (RebuildTask.debug task)

installResult ::
  NvimE e m =>
  MonadBaseControl IO m =>
  MonadIO m =>
  RebuildTask ->
  InstallResult ->
  m RunBuiltResult
installResult task (InstallResult.Success rplugin) = installSuccess task rplugin
installResult task (InstallResult.Failure err) = return $ RunBuiltResult.Failure task err

handleNonexisting ::
  MonadRibo m =>
  NvimE e m =>
  MonadMask m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  RunExistingResult ->
  m RunBuiltResult
handleNonexisting (RunExistingResult.Success active) = return $ RunBuiltResult.Success active
handleNonexisting (RunExistingResult.Failure task err) =
  return $ RunBuiltResult.PreviousFailure "run-existing" task err
handleNonexisting (RunExistingResult.NotReady task@(RebuildTask name source _ _)) = do
  result <- installRplugin name source
  installResult task result
