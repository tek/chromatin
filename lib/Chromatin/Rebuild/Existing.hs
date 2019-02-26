module Chromatin.Rebuild.Existing(
  handleExisting,
  stackRpluginReady,
) where

import qualified Data.ByteString.Lazy.Internal as B (unpackChars)
import Data.List.Split (linesBy)
import GHC.IO.Exception (IOException)
import qualified Ribosome.Log as Log (debugR)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.FilePath ((</>))
import System.Process.Typed (readProcessStderr, proc, setWorkingDir)
import UnliftIO (tryIO)
import UnliftIO.Directory (doesPathExist)

import Chromatin.Data.Chromatin (Chromatin)
import Chromatin.Data.RebuildTask (RebuildTask(..))
import Chromatin.Data.Rplugin (Rplugin(Rplugin))
import Chromatin.Data.RpluginName (RpluginName)
import Chromatin.Data.RpluginSource (RpluginSource(Stack, Pypi))
import Chromatin.Data.RpluginState (RpluginState)
import qualified Chromatin.Data.RpluginState as RpluginState (RpluginState(..))
import Chromatin.Data.RunExistingResult (RunExistingResult)
import qualified Chromatin.Data.RunExistingResult as RunExistingResult (RunExistingResult(..))
import Chromatin.Git (gitRefFromRepo, gitRefFromCache)
import Chromatin.Run (runRplugin, RunRpluginResult, pypiPluginPackage)
import qualified Chromatin.Run as RunRpluginResult (RunRpluginResult(..))

runPreexistingResult :: RebuildTask -> RunRpluginResult -> RunExistingResult
runPreexistingResult _ (RunRpluginResult.Success active) = RunExistingResult.Success active
runPreexistingResult task (RunRpluginResult.Failure err) = RunExistingResult.Failure task err

unsafeStackDryRun :: FilePath -> Chromatin (ExitCode, [String])
unsafeStackDryRun path = do
  result <- readProcessStderr $ setWorkingDir path $ proc "stack" ["build", "--dry-run"]
  return $ linesBy (=='\n') . B.unpackChars <$> result

stackDryRun :: FilePath -> Chromatin (Either IOException (ExitCode, [String]))
stackDryRun path =
  tryIO $ unsafeStackDryRun path

stackRpluginReadyFromBuild :: FilePath -> Chromatin RpluginState
stackRpluginReadyFromBuild path = do
  isStackRepo <- doesPathExist $ path </> "stack.yaml"
  if isStackRepo then check else return $ RpluginState.Broken $ "not a stack package: " ++ path
  where
    check = do
      output <- stackDryRun path
      return $ case output of
        Right (ExitSuccess, lines') ->
          if "Would build:" `elem` lines'
          then RpluginState.Incomplete
          else RpluginState.Ready
        Right (ExitFailure _, lines') ->
          RpluginState.Broken $ "stack execution failed in " ++ path ++ ": " ++ show lines'
        Left err ->
          RpluginState.Broken $ "could not execute `stack build` in " ++ path ++ ": " ++ show err

stackRpluginReadyFromGit :: RpluginName -> FilePath -> Chromatin RpluginState
stackRpluginReadyFromGit name path = do
  stored <- gitRefFromCache name
  current <- gitRefFromRepo path
  maybe (stackRpluginReadyFromBuild path) (checkRef stored) current
  where
    checkRef stored current =
      return $ if current `elem` stored then RpluginState.Ready else RpluginState.Incomplete

stackRpluginReadyFromGitOrBuild :: RpluginName -> FilePath -> Chromatin RpluginState
stackRpluginReadyFromGitOrBuild name path = do
  isGitRepo <- doesPathExist $ path </> ".git"
  if isGitRepo then stackRpluginReadyFromGit name path else stackRpluginReadyFromBuild path

stackRpluginReady :: RpluginName -> FilePath -> Bool -> Chromatin RpluginState
stackRpluginReady name path =
  checkDev
  where
    checkDev True = stackRpluginReadyFromBuild path
    checkDev False = stackRpluginReadyFromGitOrBuild name path

rpluginReady :: RpluginName -> RpluginSource -> Bool -> Chromatin RpluginState
rpluginReady name (Stack path) dev = stackRpluginReady name path dev
rpluginReady name (Pypi _) _ = do
  package <- pypiPluginPackage name
  return $ maybe RpluginState.Incomplete (const RpluginState.Ready) package
rpluginReady _ source _ = return $ RpluginState.Broken $ "NI: rpluginReady for " ++ show source

runPreexisting :: RebuildTask -> Chromatin RunExistingResult
runPreexisting task@(RebuildTask name source _) =
  runPreexistingResult task <$> runRplugin (Rplugin name source)

handleExisting :: RebuildTask -> Chromatin RunExistingResult
handleExisting task@(RebuildTask name source dev) = do
  Log.debugR $ "handling " ++ show task
  state <- rpluginReady name source dev
  case state of
    RpluginState.Ready -> runPreexisting task
    RpluginState.Incomplete -> return $ RunExistingResult.NotReady task
    RpluginState.Broken reason -> return $ RunExistingResult.Failure task [reason]
