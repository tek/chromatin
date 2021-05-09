module Chromatin.Rebuild.Build where

import Control.Monad.Catch (MonadMask, MonadThrow)
import Path (Abs, Dir, File, Path, reldir, toFilePath)
import Path.IO (XdgDirectory(XdgData), createDirIfMissing, getXdgDir, withSystemTempFile)
import Ribosome.Api.Echo (echom)
import Ribosome.Data.PersistError (PersistError)
import Ribosome.Data.SettingError (SettingError)
import qualified Ribosome.Log as Log (debug)
import Ribosome.Nvim.Api.Data (Buffer)
import Ribosome.Nvim.Api.IO (bufferGetNumber, vimCommand, vimGetCurrentBuffer)
import System.Exit (ExitCode(ExitFailure))
import System.Process.Typed (ProcessConfig, proc, runProcess, setStderr, setStdout, setWorkingDir, shell, useHandleClose)

import Chromatin.Data.Rplugin (Rplugin(Rplugin))
import Chromatin.Data.RpluginName (RpluginName(..))
import Chromatin.Data.RpluginSource (FlakeUrl(FlakeUrl), HackageDepspec(..), RpluginSource(Hackage, Stack, Flake))
import Chromatin.Git (gitRefFromRepo, storeProjectRef)

data InstallResult =
  Success Rplugin
  |
  Failure [Text]
  deriving (Show, Eq)

tailInTerminal ::
  NvimE e m =>
  Path Abs File ->
  m Buffer
tailInTerminal path = do
  vimCommand $ "belowright 15split term://tail -f " <> toText (toFilePath path)
  vimGetCurrentBuffer <* vimCommand "normal! G" <* vimCommand "wincmd w"

closeTerminal ::
  NvimE e m =>
  Buffer ->
  m ()
closeTerminal buf = do
  num <- bufferGetNumber buf
  _ <- vimCommand $ "silent! " <> show num <> "bwipeout!"
  return ()

processWithOutFile :: MonadIO m => ProcessConfig stdin stdout stderr -> Handle -> m (Either Text ())
processWithOutFile processConfig logHandle = do
  code <- runProcess $ pipe processConfig
  return $ case code of
    ExitFailure _ -> Left "installation process failed"
    _ -> Right ()
  where
    stream = useHandleClose logHandle
    pipe = setStdout stream . setStderr stream

hackageProcess ::
  Path Abs Dir ->
  HackageDepspec ->
  ProcessConfig () () ()
hackageProcess bindir (HackageDepspec spec) =
  proc "cabal" ["new-install", "--symlink-bindir", toFilePath bindir, toString spec]

stackProcess :: Path Abs Dir -> ProcessConfig () () ()
stackProcess path =
  setWorkingDir (toFilePath path) $ shell "unset STACK_IN_NIX_SHELL; stack build"

flakeProcess :: FlakeUrl -> ProcessConfig () () ()
flakeProcess (FlakeUrl url) =
  shell [text|nix -L --refresh build --no-link #{url}|]

processWithLog ::
  MonadRibo m =>
  NvimE e m =>
  MonadMask m =>
  RpluginName ->
  ProcessConfig stdin stdout sderr ->
  m (Either Text ())
processWithLog (RpluginName name) processConfig =
  withSystemTempFile "chromatin-install" $ \logFile logHandle -> do
    buf <- tailInTerminal logFile
    result <- processWithOutFile processConfig logHandle
    case result of
      Right _ -> do
        echom [text|installed `#{name}`|]
        closeTerminal buf
      Left _ ->
        echom [text|failed to install `#{name}`|]
    pure result

installHackageProcess ::
  MonadRibo m =>
  NvimE e m =>
  MonadMask m =>
  RpluginName ->
  Path Abs Dir ->
  HackageDepspec ->
  m (Either Text ())
installHackageProcess name bindir spec =
  processWithLog name (hackageProcess bindir spec)

installStackProcess ::
  MonadRibo m =>
  NvimE e m =>
  MonadMask m =>
  RpluginName ->
  Path Abs Dir ->
  m (Either Text ())
installStackProcess name path =
  processWithLog name (stackProcess path)

installFlakeProcess ::
  MonadRibo m =>
  NvimE e m =>
  MonadMask m =>
  RpluginName ->
  FlakeUrl ->
  m (Either Text ())
installFlakeProcess name url =
  processWithLog name $ flakeProcess url

binaryDir ::
  MonadIO m =>
  m (Path Abs Dir)
binaryDir =
  getXdgDir XdgData (Just [reldir|chromatin/bin|])

installRpluginFromSource ::
  MonadRibo m =>
  NvimE e m =>
  MonadMask m =>
  RpluginName ->
  RpluginSource ->
  m (Either Text (Maybe (Path Abs Dir)))
installRpluginFromSource name = \case
  Hackage spec -> do
    bindir <- binaryDir
    createDirIfMissing True bindir
    result <- installHackageProcess name bindir spec
    return (Nothing <$ result)
  Stack path -> do
    result <- installStackProcess name path
    return (Just path <$ result)
  Flake url -> do
    result <- installFlakeProcess name url
    pure (Nothing <$ result)

updateProjectRef ::
  MonadRibo m =>
  NvimE e m =>
  MonadThrow m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  RpluginName ->
  Path Abs Dir ->
  m ()
updateProjectRef name path = do
  ref <- gitRefFromRepo path
  traverse_ (storeProjectRef name) ref

installRplugin ::
  MonadRibo m =>
  NvimE e m =>
  MonadMask m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  RpluginName ->
  RpluginSource ->
  m InstallResult
installRplugin name@(RpluginName n) source = do
  Log.debug $ "installing " <> n
  result <- installRpluginFromSource name source
  Log.debug $ "installed " <> n <> ": " <> show result
  case result of
    Right path -> do
      traverse_ (updateProjectRef name) path
      return $ Success $ Rplugin name source
    Left err ->
      pure $ Failure $ lines err
