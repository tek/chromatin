module Chromatin.Run where

import Control.Exception.Lifted (catch)
import qualified Data.Map as Map (singleton)
import Data.MessagePack (Object)
import Neovim (NeovimException)
import qualified Neovim as NeovimException (NeovimException(ErrorMessage, ErrorResult))
import Path (Abs, Dir, Path, absdir, toFilePath)
import Path.IO (createDirIfMissing)
import Ribosome.Msgpack.Encode (toMsgpack)
import Ribosome.Nvim.Api.IO (vimCallFunction)

import Chromatin.Data.ActiveRplugin (ActiveRplugin(ActiveRplugin))
import Chromatin.Data.Rplugin (Rplugin(Rplugin))
import Chromatin.Data.RpluginName (RpluginName(RpluginName))
import Chromatin.Data.RpluginSource (
  FlakeUrl(FlakeUrl),
  HackageDepspec(HackageDepspec),
  RpluginSource(Flake, Stack, Hackage),
  )
import Chromatin.Rebuild.Build (binaryDir)

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

runRpluginChromatin ::
  NvimE e m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  RpluginName ->
  Text ->
  Map ByteString Object ->
  Bool ->
  m (Either Text Int)
runRpluginChromatin (RpluginName name) cmd extraOpts debug = do
  createDirIfMissing False [absdir|/tmp/chromatin-debug|]
  jobstart [toMsgpack @Text [text|#{cmd} #{logParam}|], toMsgpack (opts <> extraOpts)]
  where
    opts =
      Map.singleton "rpc" (toMsgpack True)
    logParam
      | debug = "-- -v DEBUG -l /tmp/chromatin-debug/" <> name
      | otherwise = ""

runRpluginStack ::
  NvimE e m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  RpluginName ->
  Path Abs Dir ->
  Bool ->
  m (Either Text Int)
runRpluginStack rplugin path debug = do
  runRpluginChromatin rplugin "unset STACK_IN_NIX_SHELL; stack exec " extraOpts debug
  where
    extraOpts =
      Map.singleton "cwd" (toMsgpack (toFilePath path))

runRpluginFlake ::
  NvimE e m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  RpluginName ->
  FlakeUrl ->
  Bool ->
  m (Either Text Int)
runRpluginFlake rplugin (FlakeUrl url) debug =
  runRpluginChromatin rplugin [text|nix run #{url}|] mempty debug

runRpluginHackage ::
  NvimE e m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  RpluginName ->
  Bool ->
  m (Either Text Int)
runRpluginHackage rplugin@(RpluginName name) debug = do
  bindir <- binaryDir
  runRpluginChromatin rplugin [text|#{toFilePath bindir}/#{name}|] mempty debug

runRplugin' ::
  NvimE e m =>
  MonadBaseControl IO m =>
  MonadIO m =>
  RpluginName ->
  Bool ->
  RpluginSource ->
  m (Either Text Int)
runRplugin' name debug = \case
  Stack path ->
    runRpluginStack name path debug
  Flake url ->
    runRpluginFlake name url debug
  Hackage (HackageDepspec _) ->
    runRpluginHackage name debug
  _ ->
    return (Left "NI")

runRplugin ::
  NvimE e m =>
  MonadBaseControl IO m =>
  MonadIO m =>
  Rplugin ->
  Bool ->
  m RunRpluginResult
runRplugin rplugin@(Rplugin name source) debug = do
  runRplugin' name debug source <&> \case
    Right channelId -> Success (ActiveRplugin channelId rplugin)
    Left err -> Failure (lines err)
