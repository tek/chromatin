module Chromatin.Rebuild.Nonexisting(
  handleNonexisting,
) where

import Chromatin.Data.Chromatin (Chromatin)
import Chromatin.Data.Rplugin (Rplugin)
import Chromatin.Data.RunExistingResult (RunExistingResult)
import qualified Chromatin.Data.RunExistingResult as RunExistingResult (RunExistingResult(..))
import Chromatin.Data.RunBuiltResult (RunBuiltResult)
import qualified Chromatin.Data.RunBuiltResult as RunBuiltResult (RunBuiltResult(..))
import Chromatin.Data.RebuildTask (RebuildTask(..))
import Chromatin.Rebuild.Build (InstallResult, installRplugin)
import qualified Chromatin.Rebuild.Build as InstallResult (InstallResult(Success, Failure))
import Chromatin.Run (runRplugin, RunRpluginResult)
import qualified Chromatin.Run as RunRpluginResult (RunRpluginResult(..))

runBuiltResult :: RebuildTask -> RunRpluginResult -> RunBuiltResult
runBuiltResult _ (RunRpluginResult.Success active) = RunBuiltResult.Success active
runBuiltResult task (RunRpluginResult.Failure err) = RunBuiltResult.Failure task err

installSuccess :: RebuildTask -> Rplugin -> Chromatin RunBuiltResult
installSuccess task rplugin =
  fmap (runBuiltResult task) (runRplugin rplugin)

installResult :: RebuildTask -> InstallResult -> Chromatin RunBuiltResult
installResult task (InstallResult.Success rplugin) = installSuccess task rplugin
installResult task (InstallResult.Failure err) = return $ RunBuiltResult.Failure task err

handleNonexisting :: RunExistingResult -> Chromatin RunBuiltResult
handleNonexisting (RunExistingResult.Success active) = return $ RunBuiltResult.Success active
handleNonexisting (RunExistingResult.Failure task err) =
  return $ RunBuiltResult.PreviousFailure "run-existing" task err
handleNonexisting (RunExistingResult.NotReady task@(RebuildTask name source)) = do
  result <- installRplugin name source
  installResult task result
