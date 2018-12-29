module Chromatin.Rebuild.Nonexisting(
  handleNonexisting,
) where

import Chromatin.Data.Chromatin (Chromatin)
import Chromatin.Data.Rplugin (Rplugin)
import Chromatin.Data.RunExistingResult (RunExistingResult)
import qualified Chromatin.Data.RunExistingResult as RunExistingResult (RunExistingResult(..))
import Chromatin.Data.RunInstalledResult (RunInstalledResult)
import qualified Chromatin.Data.RunInstalledResult as RunInstalledResult (RunInstalledResult(..))
import Chromatin.Data.RebuildTask (RebuildTask(..))
import Chromatin.Rebuild.Install (InstallResult, installRplugin)
import qualified Chromatin.Rebuild.Install as InstallResult (InstallResult(Success, Failure))
import Chromatin.Run (runRplugin, RunRpluginResult)
import qualified Chromatin.Run as RunRpluginResult (RunRpluginResult(..))

runInstalledResult :: RebuildTask -> RunRpluginResult -> RunInstalledResult
runInstalledResult _ (RunRpluginResult.Success active) = RunInstalledResult.Success active
runInstalledResult task (RunRpluginResult.Failure err) = RunInstalledResult.Failure task err

installSuccess :: RebuildTask -> Rplugin -> Chromatin RunInstalledResult
installSuccess task rplugin =
  fmap (runInstalledResult task) (runRplugin rplugin)

installResult :: RebuildTask -> InstallResult -> Chromatin RunInstalledResult
installResult task (InstallResult.Success rplugin) = installSuccess task rplugin
installResult task (InstallResult.Failure err) = return $ RunInstalledResult.Failure task err

handleNonexisting :: RunExistingResult -> Chromatin RunInstalledResult
handleNonexisting (RunExistingResult.Success active) = return $ RunInstalledResult.Success active
handleNonexisting (RunExistingResult.Failure task err) =
  return $ RunInstalledResult.PreviousFailure "run-existing" task err
handleNonexisting (RunExistingResult.NotReady task@(RebuildTask name source)) = do
  result <- installRplugin name source
  installResult task result
