module Chromatin.Git where

import Control.Monad.Trans.Except (runExceptT)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B (stripSuffix)
import qualified Data.ByteString.Lazy.Internal as B (unpackChars)
import Data.Either (fromRight)
import Data.Map (Map, (!?))
import qualified Data.Map as Map (empty, insert)
import Data.Maybe (fromMaybe)
import Ribosome.Persist (persistLoad, persistStore)
import System.Exit (ExitCode(ExitSuccess))
import System.Process.Typed (proc, readProcessStdout)
import UnliftIO.Exception (tryAny)

import Chromatin.Data.Chromatin (Chromatin)
import Chromatin.Data.RpluginName (RpluginName(RpluginName))

chomp :: ByteString -> ByteString
chomp s =
  fromMaybe s $ B.stripSuffix "\n" s

persistName :: FilePath
persistName = "project-refs"

loadProjectRefs :: Chromatin (Map String String)
loadProjectRefs =
  fromRight Map.empty <$> runExceptT (persistLoad persistName)

storeProjectRef :: RpluginName -> String -> Chromatin ()
storeProjectRef (RpluginName name) ref = do
  current <- loadProjectRefs
  persistStore persistName $ Map.insert name ref current

gitRefFromCache :: RpluginName -> Chromatin (Maybe String)
gitRefFromCache (RpluginName name) = do
  refs <- loadProjectRefs
  return $ refs !? name

gitRefFromRepo :: FilePath -> Chromatin (Maybe String)
gitRefFromRepo path = do
  result <- tryAny $ readProcessStdout $ proc "git" ["-C", path, "rev-parse", "HEAD"]
  return $ either (const Nothing) decode result
  where
    decode (code, out) = if code == ExitSuccess then Just . B.unpackChars . chomp $ out else Nothing
