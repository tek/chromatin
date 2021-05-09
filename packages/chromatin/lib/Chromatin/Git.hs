module Chromatin.Git where

import Control.Exception.Lifted (try)
import Control.Monad.Catch (MonadThrow)
import qualified Data.ByteString.Lazy as B (stripSuffix)
import Data.Map ((!?))
import qualified Data.Map as Map (empty, insert)
import Path (Abs, Dir, File, Path, Rel, relfile, toFilePath)
import Ribosome.Data.PersistError (PersistError)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Persist (mayPersistLoad, persistStore)
import System.Exit (ExitCode(ExitSuccess))
import System.Process.Typed (proc, readProcessStdout)

import Chromatin.Data.RpluginName (RpluginName(RpluginName))

chomp :: LByteString -> LByteString
chomp s =
  fromMaybe s $ B.stripSuffix "\n" s

persistName :: Path Rel File
persistName =
  [relfile|project-refs|]

loadProjectRefs ::
  MonadRibo m =>
  NvimE e m =>
  MonadThrow m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  m (Map Text Text)
loadProjectRefs =
  fromMaybe Map.empty <$> mayPersistLoad persistName

storeProjectRef ::
  MonadRibo m =>
  NvimE e m =>
  MonadThrow m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  RpluginName ->
  Text ->
  m ()
storeProjectRef (RpluginName name) ref = do
  current <- loadProjectRefs
  persistStore persistName $ Map.insert name ref current

gitRefFromCache ::
  MonadRibo m =>
  NvimE e m =>
  MonadThrow m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  RpluginName ->
  m (Maybe Text)
gitRefFromCache (RpluginName name) = do
  refs <- loadProjectRefs
  return $ refs !? name

gitRefFromRepo ::
  ∀ m.
  MonadIO m =>
  MonadBaseControl IO m =>
  Path Abs Dir ->
  m (Maybe Text)
gitRefFromRepo path = do
  result <- try @m @SomeException $ readProcessStdout $ proc "git" ["-C", toFilePath path, "rev-parse", "HEAD"]
  return $ either (const Nothing) decode result
  where
    decode (code, out) =
      if code == ExitSuccess then Just . decodeUtf8 . chomp $ out else Nothing
