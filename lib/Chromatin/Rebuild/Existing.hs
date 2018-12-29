module Chromatin.Rebuild.Existing(
  handleExisting,
) where

import GHC.IO.Exception (IOException)
import qualified Data.ByteString.Lazy.Internal as B (unpackChars)
import Data.List.Split (linesBy)
import System.Process.Typed (readProcessStderr, proc, setWorkingDir)
import UnliftIO (tryIO)
import Chromatin.Data.Chromatin (Chromatin)
import Chromatin.Data.RebuildTask (RebuildTask(..))
import Chromatin.Data.RunExistingResult (RunExistingResult)
import qualified Chromatin.Data.RunExistingResult as RunExistingResult (RunExistingResult(..))
import Chromatin.Data.Rplugin (Rplugin(Rplugin))
import Chromatin.Data.RpluginName (RpluginName)
import Chromatin.Data.RpluginSource (RpluginSource(Stack, Pypi))
import Chromatin.Data.RpluginState (RpluginState)
import qualified Chromatin.Data.RpluginState as RpluginState (RpluginState(..))
import Chromatin.Run (runRplugin, RunRpluginResult, pypiPluginPackage)
import qualified Chromatin.Run as RunRpluginResult (RunRpluginResult(..))

runPreexistingResult :: RebuildTask -> RunRpluginResult -> RunExistingResult
runPreexistingResult _ (RunRpluginResult.Success active) = RunExistingResult.Success active
runPreexistingResult task (RunRpluginResult.Failure err) = RunExistingResult.Failure task err

unsafeStackDryRun :: FilePath -> Chromatin [String]
unsafeStackDryRun path = do
  (_, output) <- readProcessStderr $ setWorkingDir path $ proc "stack" ["build", "--dry-run"]
  return $ linesBy (=='\n') $ B.unpackChars output

stackDryRun :: FilePath -> Chromatin (Either IOException [String])
stackDryRun path =
  tryIO $ unsafeStackDryRun path

rpluginReady :: RpluginName -> RpluginSource -> Chromatin RpluginState
rpluginReady _ (Stack path) = do
  output <- stackDryRun path
  return $ case output of
    Right lines' -> if "Would build:" `elem` lines' then RpluginState.Incomplete else RpluginState.Ready
    Left err -> RpluginState.Broken $ "could not execute `stack build` in " ++ path ++ ": " ++ show err
rpluginReady name (Pypi _) = do
  package <- pypiPluginPackage name
  return $ maybe RpluginState.Incomplete (const RpluginState.Ready) package
rpluginReady _ _ = return $ RpluginState.Broken "NI"

runPreexisting :: RebuildTask -> Chromatin RunExistingResult
runPreexisting task@(RebuildTask name source) =
  runPreexistingResult task <$> runRplugin (Rplugin name source)

handleExisting :: RebuildTask -> Chromatin RunExistingResult
handleExisting task@(RebuildTask name source) = do
  state <- rpluginReady name source
  case state of
    RpluginState.Ready -> runPreexisting task
    RpluginState.Incomplete -> return $ RunExistingResult.NotReady task
    RpluginState.Broken reason -> return $ RunExistingResult.Failure task [reason]
