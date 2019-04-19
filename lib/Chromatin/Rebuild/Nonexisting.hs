module Chromatin.Rebuild.Nonexisting where

import Chromatin.Data.Chromatin (Chromatin)
import Chromatin.Data.RebuildTask (RebuildTask(RebuildTask))
import qualified Chromatin.Data.RebuildTask as RebuildTask (RebuildTask(debug))
import Chromatin.Data.Rplugin (Rplugin)
import Chromatin.Data.RunBuiltResult (RunBuiltResult)
import qualified Chromatin.Data.RunBuiltResult as RunBuiltResult (RunBuiltResult(..))
import Chromatin.Data.RunExistingResult (RunExistingResult)
import qualified Chromatin.Data.RunExistingResult as RunExistingResult (RunExistingResult(..))
import qualified Chromatin.Log as Log
import Chromatin.Rebuild.Build (InstallResult, installRplugin)
import qualified Chromatin.Rebuild.Build as InstallResult (InstallResult(Success, Failure))
import Chromatin.Run (RunRpluginResult, runRplugin)
import qualified Chromatin.Run as RunRpluginResult (RunRpluginResult(..))

runBuiltResult :: RebuildTask -> RunRpluginResult -> RunBuiltResult
runBuiltResult _ (RunRpluginResult.Success active) = RunBuiltResult.Success active
runBuiltResult task (RunRpluginResult.Failure err) = RunBuiltResult.Failure task err

installSuccess :: RebuildTask -> Rplugin -> Chromatin RunBuiltResult
installSuccess task rplugin =
  runBuiltResult task <$> runRplugin rplugin (RebuildTask.debug task)

installResult :: RebuildTask -> InstallResult -> Chromatin RunBuiltResult
installResult task (InstallResult.Success rplugin) = installSuccess task rplugin
installResult task (InstallResult.Failure err) = return $ RunBuiltResult.Failure task err

handleNonexisting :: RunExistingResult -> Chromatin RunBuiltResult
handleNonexisting (RunExistingResult.Success active) = return $ RunBuiltResult.Success active
handleNonexisting (RunExistingResult.Failure task err) =
  return $ RunBuiltResult.PreviousFailure "run-existing" task err
handleNonexisting (RunExistingResult.NotReady task@(RebuildTask name source _ _)) = do
  result <- installRplugin name source
  installResult task result
