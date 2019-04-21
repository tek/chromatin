{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ExistingSpec(
  htf_thisModulesTests,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor (void)
import Path (Abs, Dir, File, Path, Rel, parseAbsDir, relfile, toFilePath, (</>))
import Ribosome.Test.Unit (tempDir)
import System.Process.Typed (proc, readProcess_, setWorkingDir)
import Test.Framework

import Chromatin.Data.Chromatin (ChromatinN)
import Chromatin.Data.RpluginName (RpluginName(..))
import Chromatin.Data.RpluginState (RpluginState(..))
import Chromatin.Git (gitRefFromRepo, storeProjectRef)
import Chromatin.Rebuild.Existing (stackRpluginReady)
import Chromatin.Test.Unit (specWithDef)
import Config (vars)
import Test ()

git :: MonadIO m => Path Abs Dir -> [Text] -> m ()
git repoDir args = liftIO $ void $ readProcess_ (setWorkingDir (toFilePath repoDir) $ proc "git" (toString <$> args))

addFile :: MonadIO m => Path Abs Dir -> Path Rel File -> m ()
addFile repoDir s = do
  liftIO $ writeFile repoFP fileS
  git repoDir ["add", fileS]
  git repoDir ["commit", "-m", "commit"]
  where
    repoFP = toFilePath (repoDir </> s)
    fileS = (toText . toFilePath) s

name :: RpluginName
name = RpluginName "proj"

setRef :: Path Abs Dir -> ChromatinN ()
setRef repoDir = do
  mayRef <- gitRefFromRepo repoDir
  ref <- gassertJust mayRef
  storeProjectRef name ref

existingSpec :: ChromatinN ()
existingSpec = do
  repoDirFP <- tempDir "existing/repo"
  repoDir <- parseAbsDir repoDirFP
  git repoDir ["init", "-q"]
  addFile repoDir [relfile|test|]
  setRef repoDir
  ready <- stackRpluginReady name repoDir False
  liftIO $ assertEqual Ready ready
  addFile repoDir [relfile|test2|]
  incomplete <- stackRpluginReady name repoDir False
  liftIO $ assertEqual Incomplete incomplete

test_existing :: IO ()
test_existing =
  vars >>= specWithDef existingSpec

noGitSpec :: ChromatinN ()
noGitSpec = do
  repoDirFP <- tempDir "existing/repo"
  repoDir <- parseAbsDir repoDirFP
  broken <- stackRpluginReady name repoDir False
  gassertEqual (Broken $ "not a stack package: " <> (toText repoDirFP) <> "/") broken

test_noGit :: IO ()
test_noGit =
  vars >>= specWithDef noGitSpec
