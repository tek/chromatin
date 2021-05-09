module Chromatin.Rebuild.Existing where

import Control.Exception.Lifted (IOException, try)
import Control.Monad.Catch (MonadThrow)
import Path (Abs, Dir, Path, relfile, toFilePath, (</>))
import Path.IO (doesFileExist, isLocationOccupied)
import Ribosome.Data.PersistError (PersistError)
import Ribosome.Data.SettingError (SettingError)
import qualified Ribosome.Log as Log (debug)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Process.Typed (readProcessInterleaved, readProcessStderr, setWorkingDir, shell)

import Chromatin.Data.RebuildTask (RebuildTask(..))
import Chromatin.Data.Rplugin (Rplugin(Rplugin))
import Chromatin.Data.RpluginName (RpluginName)
import Chromatin.Data.RpluginSource (FlakeUrl(FlakeUrl), RpluginSource(Stack, Flake))
import Chromatin.Data.RpluginState (RpluginState)
import qualified Chromatin.Data.RpluginState as RpluginState (RpluginState(..))
import Chromatin.Data.RunExistingResult (RunExistingResult)
import qualified Chromatin.Data.RunExistingResult as RunExistingResult (RunExistingResult(..))
import Chromatin.Git (gitRefFromCache, gitRefFromRepo)
import Chromatin.Run (RunRpluginResult, runRplugin)
import qualified Chromatin.Run as RunRpluginResult (RunRpluginResult(..))

runPreexistingResult :: RebuildTask -> RunRpluginResult -> RunExistingResult
runPreexistingResult _ (RunRpluginResult.Success active) = RunExistingResult.Success active
runPreexistingResult task (RunRpluginResult.Failure err) = RunExistingResult.Failure task err

unsafeStackDryRun ::
  MonadIO m =>
  Path Abs Dir ->
  m (ExitCode, [Text])
unsafeStackDryRun path = do
  result <- readProcessStderr conf
  return $ lines . decodeUtf8 <$> result
  where
    conf = setWorkingDir (toFilePath path) $ shell "unset STACK_IN_NIX_SHELL; stack --no-install-ghc build --dry-run"

stackDryRun ::
  MonadIO m =>
  MonadBaseControl IO m =>
  Path Abs Dir ->
  m (Either IOException (ExitCode, [Text]))
stackDryRun path =
  try $ unsafeStackDryRun path

stackRpluginReadyFromBuild ::
  MonadIO m =>
  MonadBaseControl IO m =>
  Path Abs Dir ->
  m RpluginState
stackRpluginReadyFromBuild path = do
  isStackRepo <- doesFileExist $ path </> [relfile|stack.yaml|]
  if isStackRepo then check else return $ RpluginState.Broken $ "not a stack package: " <> (toText . toFilePath) path
  where
    check = do
      output <- stackDryRun path
      return $ case output of
        Right (ExitSuccess, lines') ->
          if "Would build:" `elem` lines'
          then RpluginState.Incomplete
          else RpluginState.Ready
        Right (ExitFailure _, lines') ->
          RpluginState.Broken $ "stack execution failed in " <> (toText . toFilePath) path <> ": " <> show lines'
        Left err ->
          RpluginState.Broken $ "could not execute `stack build` in " <> (toText . toFilePath) path <> ": " <> show err

stackRpluginReadyFromGit ::
  NvimE e m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  RpluginName ->
  Path Abs Dir ->
  m RpluginState
stackRpluginReadyFromGit name path = do
  stored <- gitRefFromCache name
  current <- gitRefFromRepo path
  maybe (stackRpluginReadyFromBuild path) (checkRef stored) current
  where
    checkRef stored current =
      return $ if current `elem` stored then RpluginState.Ready else RpluginState.Incomplete

stackRpluginReadyFromGitOrBuild ::
  NvimE e m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  RpluginName ->
  Path Abs Dir ->
  m RpluginState
stackRpluginReadyFromGitOrBuild name path = do
  isGitRepo <- isLocationOccupied $ path </> [relfile|.git|]
  if isGitRepo then stackRpluginReadyFromGit name path else stackRpluginReadyFromBuild path

stackRpluginReady ::
  NvimE e m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  RpluginName ->
  Path Abs Dir ->
  Bool ->
  m RpluginState
stackRpluginReady name path = \case
  True -> stackRpluginReadyFromBuild path
  False -> stackRpluginReadyFromGitOrBuild name path

-- |Check whether a flake url needs rebuilding.
-- There are multiple possibilities:
--  - The command fails because of IFD, which might be a bug according to a GH issue
--  - The command prints what would be built
--  - The command prints nothing when the local build is up to date
flakeRpluginReady ::
  MonadIO m =>
  FlakeUrl ->
  m RpluginState
flakeRpluginReady (FlakeUrl url) =
  readProcessInterleaved (shell [text|nix --refresh build --dry-run #{url}|]) <&> \case
    (ExitSuccess, []) -> RpluginState.Ready
    _ -> RpluginState.Incomplete

rpluginReady ::
  NvimE e m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  RpluginName ->
  Bool ->
  RpluginSource ->
  m RpluginState
rpluginReady name dev = \case
  Stack path ->
    stackRpluginReady name path dev
  Flake url ->
    flakeRpluginReady url
  source ->
    pure $ RpluginState.Broken $ "NI: rpluginReady for " <> show source

runPreexisting ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  RebuildTask ->
  m RunExistingResult
runPreexisting task@(RebuildTask name source _ debug) =
  runPreexistingResult task <$> runRplugin (Rplugin name source) debug

handleExisting ::
  NvimE e m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  RebuildTask ->
  m RunExistingResult
handleExisting task@(RebuildTask name source dev _) = do
  Log.debug $ ("handling " :: Text) <> show task
  status <- rpluginReady name dev source
  case status of
    RpluginState.Ready -> runPreexisting task
    RpluginState.Incomplete -> return $ RunExistingResult.NotReady task
    RpluginState.Broken reason -> return $ RunExistingResult.Failure task [reason]
