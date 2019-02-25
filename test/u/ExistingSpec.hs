{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ExistingSpec(
  htf_thisModulesTests,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor (void)
import Ribosome.Test.Unit (tempDir)
import System.FilePath ((</>))
import System.Process.Typed (readProcess_, proc, setWorkingDir)
import Test.Framework

import Chromatin.Data.Chromatin (Chromatin)
import Chromatin.Data.RpluginName (RpluginName(..))
import Chromatin.Data.RpluginState (RpluginState(..))
import Chromatin.Git (storeProjectRef, gitRefFromRepo)
import Chromatin.Rebuild.Existing (stackRpluginReady)
import Chromatin.Test.Unit (specWithDef)
import Config (vars)
import Test ()

git :: MonadIO m => FilePath -> [String] -> m ()
git repoDir args = liftIO $ void $ readProcess_ (setWorkingDir repoDir $ proc "git" args)

addFile :: MonadIO m => FilePath -> String -> m ()
addFile repoDir s = do
  liftIO $ writeFile (repoDir </> s) s
  git repoDir ["add", s]
  git repoDir ["commit", "-m", "commit"]

name :: RpluginName
name = RpluginName "proj"

setRef :: FilePath -> Chromatin ()
setRef repoDir = do
  mayRef <- gitRefFromRepo repoDir
  ref <- gassertJust mayRef
  storeProjectRef name ref

existingSpec :: Chromatin ()
existingSpec = do
  repoDir <- tempDir "existing/repo"
  git repoDir ["init", "-q"]
  addFile repoDir "test"
  setRef repoDir
  ready <- stackRpluginReady name repoDir False
  liftIO $ assertEqual Ready ready
  addFile repoDir "test2"
  incomplete <- stackRpluginReady name repoDir False
  liftIO $ assertEqual Incomplete incomplete

test_existing :: IO ()
test_existing =
  vars >>= specWithDef existingSpec

noGitSpec :: Chromatin ()
noGitSpec = do
  repoDir <- tempDir "existing/repo"
  broken <- stackRpluginReady name repoDir False
  gassertEqual (Broken $ "not a stack package: " ++ repoDir) broken

test_noGit :: IO ()
test_noGit =
  vars >>= specWithDef noGitSpec
