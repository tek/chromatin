module Chromatin.Run(
  runRplugin,
  RunRpluginResult(..),
  pypiPluginPackage,
) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.List.Split (linesBy)
import qualified Data.Map as Map (fromList)
import Data.MessagePack (Object)
import Neovim (NeovimException, fromObject', toObject, vim_call_function')
import qualified Neovim as NeovimException (NeovimException(ErrorMessage, ErrorResult))
import System.FilePath ((</>))
import qualified System.FilePath.Glob as Glob (compile, globDir1)
import UnliftIO.Directory (createDirectoryIfMissing)
import UnliftIO.Exception (catch)

import Chromatin.Data.ActiveRplugin (ActiveRplugin(ActiveRplugin))
import Chromatin.Data.Chromatin (Chromatin)
import Chromatin.Data.Rplugin (Rplugin(Rplugin))
import Chromatin.Data.RpluginName (RpluginName(RpluginName))
import Chromatin.Data.RpluginSource (RpluginSource(Stack, Pypi))
import Chromatin.Rebuild.Build (venvDir)

data RunRpluginResult =
  Success ActiveRplugin
  |
  Failure [String]
  deriving (Show, Eq)

jobstartFailure :: NeovimException -> Either String a
jobstartFailure (NeovimException.ErrorMessage doc) = Left (show doc)
jobstartFailure (NeovimException.ErrorResult obj) = Left (show obj)

unsafeJobstart :: [Object] -> Chromatin Int
unsafeJobstart args = do
  result <- vim_call_function' "jobstart" args
  fromObject' result

jobstart :: [Object] -> Chromatin (Either String Int)
jobstart args =
  catch (Right <$> unsafeJobstart args) (return . jobstartFailure)

runRpluginStack :: RpluginName -> FilePath -> Bool -> Chromatin (Either String Int)
runRpluginStack (RpluginName name) path debug = do
  createDirectoryIfMissing False "/tmp/chromatin-debug"
  jobstart [toObject $ "stack exec " ++ name ++ logParam, toObject opts]
  where
    opts = Map.fromList [("cwd" :: ByteString, toObject path), ("rpc", toObject True)]
    logParam
      | debug = " -- -v DEBUG -l /tmp/chromatin-debug/" ++ name
      | otherwise = ""

pypiPluginExecutable :: RpluginName -> Chromatin FilePath
pypiPluginExecutable name = do
  dir <- venvDir name
  return $ dir </> "bin" </> "ribosome_start_plugin"

pypiSiteDir :: RpluginName -> Chromatin (Maybe FilePath)
pypiSiteDir name = do
  venv <- venvDir name
  matches <- liftIO $ Glob.globDir1 (Glob.compile "python3.?") (venv </> "lib")
  return $ case matches of
    dir : _ -> Just $ dir </> "site-packages"
    _ -> Nothing

pypiPluginPackage :: RpluginName -> Chromatin (Maybe FilePath)
pypiPluginPackage rpluginName@(RpluginName name) = do
  dir <- pypiSiteDir rpluginName
  return $ fmap (</> name) dir

runRpluginPypi :: RpluginName -> Chromatin (Either String Int)
runRpluginPypi rpluginName@(RpluginName name) = do
  dir <- venvDir rpluginName
  let exe = dir </> "bin" </> "python"
  let opts = Map.fromList [("rpc" :: ByteString, toObject True)]
  start <- pypiPluginExecutable rpluginName
  mayPackage <- pypiPluginPackage rpluginName
  case mayPackage of
    Just package ->
      jobstart [toObject [toObject exe, toObject start, toObject package], toObject opts]
    Nothing -> return $ Left $ "no `lib/python` directory in virtualenv for `" ++ name ++ "`"

runRplugin' :: RpluginName -> RpluginSource -> Bool -> Chromatin (Either String Int)
runRplugin' name (Stack path) debug = runRpluginStack name path debug
runRplugin' name (Pypi _) _ = runRpluginPypi name
runRplugin' _ _ _ = return (Left "NI")

runRplugin :: Rplugin -> Bool -> Chromatin RunRpluginResult
runRplugin rplugin@(Rplugin name source) debug = do
  result <- runRplugin' name source debug
  return $ case result of
    Right channelId -> Success (ActiveRplugin channelId rplugin)
    Left err -> Failure (linesBy (=='\n') err)
