module Chromatin.Run where

import Control.Exception.Lifted (catch)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.ByteString (ByteString)
import qualified Data.Map as Map (fromList)
import Data.MessagePack (Object)
import Neovim (NeovimException)
import qualified Neovim as NeovimException (NeovimException(ErrorMessage, ErrorResult))
import Path (Abs, Dir, File, Path, absdir, parseAbsDir, parseRelDir, reldir, relfile, toFilePath, (</>))
import Path.IO (createDirIfMissing)
import Ribosome.Msgpack.Encode (toMsgpack)
import Ribosome.Nvim.Api.IO (vimCallFunction)
import qualified System.FilePath.Glob as Glob (compile, globDir1)

import Chromatin.Data.ActiveRplugin (ActiveRplugin(ActiveRplugin))
import Chromatin.Data.Rplugin (Rplugin(Rplugin))
import Chromatin.Data.RpluginName (RpluginName(RpluginName))
import Chromatin.Data.RpluginSource (RpluginSource(Stack, Pypi))
import Chromatin.Rebuild.Build (venvDir)

data RunRpluginResult =
  Success ActiveRplugin
  |
  Failure [Text]
  deriving (Show, Eq)

jobstartFailure :: NeovimException -> Either Text a
jobstartFailure (NeovimException.ErrorMessage doc) = Left (show doc)
jobstartFailure (NeovimException.ErrorResult obj) = Left (show obj)

unsafeJobstart ::
  NvimE e m =>
  [Object] ->
  m Int
unsafeJobstart =
  vimCallFunction "jobstart"

jobstart ::
  NvimE e m =>
  MonadBaseControl IO m =>
  [Object] ->
  m (Either Text Int)
jobstart args =
  catch (Right <$> unsafeJobstart args) (return . jobstartFailure)

runRpluginStack ::
  NvimE e m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  RpluginName ->
  (Path Abs Dir) ->
  Bool ->
  m (Either Text Int)
runRpluginStack (RpluginName name) path debug = do
  createDirIfMissing False [absdir|/tmp/chromatin-debug|]
  jobstart [toMsgpack $ "unset STACK_IN_NIX_SHELL; stack exec " <> name <> logParam, toMsgpack opts]
  where
    opts = Map.fromList [
      ("cwd" :: ByteString, toMsgpack (toFilePath path)),
      ("rpc", toMsgpack True)
      ]
    logParam
      | debug = " -- -v DEBUG -l /tmp/chromatin-debug/" <> name
      | otherwise = ""

pypiPluginExecutable ::
  MonadIO m =>
  MonadThrow m =>
  RpluginName ->
  m (Path Abs File)
pypiPluginExecutable name = do
  dir <- venvDir name
  return $ dir </> [relfile|bin/ribosome_start_plugin|]

pypiSiteDir ::
  MonadIO m =>
  MonadThrow m =>
  RpluginName ->
  m (Maybe (Path Abs Dir))
pypiSiteDir name = do
  venv <- venvDir name
  matches <- liftIO $ Glob.globDir1 (Glob.compile "python3.?") (toFilePath $ venv </> [reldir|lib|])
  case matches of
    dir : _ -> do
      dirP <- parseAbsDir dir
      return $ Just (dirP </> [reldir|site-packages|])
    _ -> return Nothing

pypiPluginPackage ::
  MonadIO m =>
  MonadThrow m =>
  RpluginName ->
  m (Maybe (Path Abs Dir))
pypiPluginPackage rpluginName@(RpluginName name) = do
  dir <- pypiSiteDir rpluginName
  nameP <- parseRelDir (toString name)
  return $ fmap (</> nameP) dir

runRpluginPypi ::
  NvimE e m =>
  MonadBaseControl IO m =>
  MonadIO m =>
  MonadThrow m =>
  RpluginName ->
  m (Either Text Int)
runRpluginPypi rpluginName@(RpluginName name) = do
  dir <- venvDir rpluginName
  let exe = dir </> [relfile|bin/python|]
  let opts = Map.fromList [("rpc" :: ByteString, toMsgpack True)]
  start <- pypiPluginExecutable rpluginName
  mayPackage <- pypiPluginPackage rpluginName
  case mayPackage of
    Just package ->
      jobstart [toMsgpack args, toMsgpack opts]
      where
        args = [toMsgpack (toFilePath exe), toMsgpack (toFilePath start), toMsgpack (toFilePath package)]
    Nothing -> return $ Left $ "no `lib/python` directory in virtualenv for `" <> name <> "`"

runRplugin' ::
  NvimE e m =>
  MonadBaseControl IO m =>
  MonadIO m =>
  MonadThrow m =>
  RpluginName ->
  RpluginSource ->
  Bool ->
  m (Either Text Int)
runRplugin' name (Stack path) debug =
  runRpluginStack name path debug
runRplugin' name (Pypi _) _ =
  runRpluginPypi name
runRplugin' _ _ _ =
  return (Left "NI")

runRplugin ::
  NvimE e m =>
  MonadBaseControl IO m =>
  MonadIO m =>
  MonadThrow m =>
  Rplugin ->
  Bool ->
  m RunRpluginResult
runRplugin rplugin@(Rplugin name source) debug = do
  result <- runRplugin' name source debug
  return $ case result of
    Right channelId -> Success (ActiveRplugin channelId rplugin)
    Left err -> Failure (lines err)
