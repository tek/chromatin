module Chromatin.Rebuild.Build(
  installRplugin,
  InstallResult(..),
  venvDir,
) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Lazy.Internal as B (unpackChars)
import Data.Foldable (traverse_)
import Data.List.Split (linesBy)
import GHC.IO.Exception (ExitCode(ExitFailure))
import GHC.IO.Handle.Types (Handle)
import Neovim (Buffer, vim_command', vim_get_current_buffer', vim_command, buffer_get_number')
import Ribosome.Api.Echo (echom)
import qualified Ribosome.Log as Log (debugR)
import System.FilePath ((</>))
import System.Process.Typed (ProcessConfig, setStdout, useHandleClose, readProcessStderr, proc, setWorkingDir)
import UnliftIO.Directory (
  getXdgDirectory,
  XdgDirectory(XdgData, XdgCache),
  createDirectoryIfMissing,
  removePathForcibly,
  )
import UnliftIO.Temporary (withSystemTempFile)

import Chromatin.Data.Chromatin (Chromatin)
import Chromatin.Data.Rplugin (Rplugin(Rplugin))
import Chromatin.Data.RpluginName (RpluginName(..))
import Chromatin.Data.RpluginSource (RpluginSource(Hackage, Stack, Pypi), HackageDepspec(..), PypiDepspec(..))
import Chromatin.Git (gitRefFromRepo, storeProjectRef)

data InstallResult =
  Success Rplugin
  |
  Failure [String]
  deriving (Show, Eq)

tailInTerminal :: FilePath -> Chromatin Buffer
tailInTerminal path = do
  vim_command' $ "15split term://tail -f " ++ path
  vim_get_current_buffer'

closeTerminal :: Buffer -> Chromatin ()
closeTerminal buf = do
  num <- buffer_get_number' buf
  _ <- vim_command $ "silent! " ++ show num ++ "bwipeout!"
  return ()

processWithStdoutFile :: MonadIO m => ProcessConfig stdin stdout stderr -> Handle -> m (Either String ())
processWithStdoutFile processConfig logHandle = do
  let pipe = setStdout (useHandleClose logHandle)
  (code, err) <- readProcessStderr $ pipe processConfig
  return $ case code of
    ExitFailure _ -> Left (B.unpackChars err)
    _ -> Right ()

hackageProcess :: FilePath -> HackageDepspec -> ProcessConfig () () ()
hackageProcess bindir (HackageDepspec spec) =
  proc "cabal" ["new-install", "--symlink-bindir", bindir, spec]

stackProcess :: FilePath -> ProcessConfig () () ()
stackProcess path =
  setWorkingDir path $ proc "stack" ["build"]

processWithLog :: RpluginName -> ProcessConfig stdin stdout sderr -> Chromatin (Either String ())
processWithLog (RpluginName name) processConfig =
  withSystemTempFile "test-chromatin" $ \logFile logHandle -> do
    buf <- tailInTerminal logFile
    result <- processWithStdoutFile processConfig logHandle
    case result of
      Right _ -> do
        echom $ "installed `" ++ name ++ "`"
        closeTerminal buf
      Left _ -> echom $  "failed to install `" ++ name ++ "`"
    return result

installHackageProcess :: RpluginName -> FilePath -> HackageDepspec -> Chromatin (Either String ())
installHackageProcess name bindir spec =
  processWithLog name $ hackageProcess bindir spec

installStackProcess :: RpluginName -> FilePath -> Chromatin (Either String ())
installStackProcess name path =
  processWithLog name $ stackProcess path

venvProcess :: FilePath -> ProcessConfig () () ()
venvProcess dir =
  proc "python3" ["-m", "venv", dir, "--upgrade"]

venvDir :: RpluginName -> Chromatin FilePath
venvDir (RpluginName name) = getXdgDirectory XdgCache ("chromatin-hs" </> "venvs" </> name)

createVenvProcess :: RpluginName -> Chromatin (Either String FilePath)
createVenvProcess name = do
  dir <- venvDir name
  removePathForcibly dir
  r <- processWithLog name (venvProcess dir)
  return $ dir <$ r

pipProcess :: PypiDepspec -> FilePath -> ProcessConfig () () ()
pipProcess (PypiDepspec spec) venv =
  proc (venv </> "bin" </> "pip") ["install", "--no-cache", "--upgrade", spec]

installPypiProcess :: RpluginName -> PypiDepspec -> FilePath -> Chromatin (Either String ())
installPypiProcess name spec venv =
  processWithLog name (pipProcess spec venv)

binaryDir :: Chromatin FilePath
binaryDir = getXdgDirectory XdgData ("chromatin" </> "bin")

installRpluginFromSource :: RpluginName -> RpluginSource -> Chromatin (Either String (Maybe FilePath))
installRpluginFromSource name (Hackage spec) = do
  bindir <- binaryDir
  createDirectoryIfMissing True bindir
  result <- installHackageProcess name bindir spec
  return $ Nothing <$ result
installRpluginFromSource name (Stack path) = do
  result <- installStackProcess name path
  return $ Just path <$ result
installRpluginFromSource name (Pypi spec) = do
  dir <- createVenvProcess name
  result <- case dir of
    Right d -> installPypiProcess name spec d
    Left e -> return $ Left e
  return $ Nothing <$ result

updateProjectRef :: RpluginName -> FilePath -> Chromatin ()
updateProjectRef name path = do
  ref <- gitRefFromRepo path
  traverse_ (storeProjectRef name) ref

installRplugin :: RpluginName -> RpluginSource -> Chromatin InstallResult
installRplugin name@(RpluginName n) source = do
  Log.debugR $ "installing " ++ n
  result <- installRpluginFromSource name source
  Log.debugR $ "installed " ++ n ++ ": " ++ show result
  case result of
    Right path -> do
      traverse_ (updateProjectRef name) path
      return $ Success $ Rplugin name source
    Left err -> return $ Failure $ linesBy (=='\n') err
