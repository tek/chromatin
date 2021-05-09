module Chromatin.Test.ExistingTest where

import Hedgehog (evalMaybe, (===))
import Path (Abs, Dir, File, Path, Rel, parseAbsDir, relfile, toFilePath, (</>))
import Ribosome.Test.Run (UnitTest)
import Ribosome.Test.Unit (tempDir)
import System.Process.Typed (proc, readProcess_, setWorkingDir)

import Chromatin.Data.RpluginName (RpluginName(..))
import Chromatin.Data.RpluginState (RpluginState(..))
import Chromatin.Git (gitRefFromRepo, storeProjectRef)
import Chromatin.Rebuild.Existing (stackRpluginReady)
import Chromatin.Test.Config (vars)
import Chromatin.Test.Unit (ChromatinTest, specWithDef)

git :: MonadIO m => Path Abs Dir -> [Text] -> m ()
git repoDir args =
  liftIO $ void $ readProcess_ (setWorkingDir (toFilePath repoDir) $ proc "git" (toString <$> args))

addFile :: MonadIO m => Path Abs Dir -> Path Rel File -> m ()
addFile repoDir s = do
  liftIO $ writeFile repoFP fileS
  git repoDir ["add", toText fileS]
  git repoDir ["commit", "-m", "commit"]
  where
    repoFP = toFilePath (repoDir </> s)
    fileS = toFilePath s

name :: RpluginName
name = RpluginName "proj"

setRef :: Path Abs Dir -> ChromatinTest ()
setRef repoDir = do
  mayRef <- gitRefFromRepo repoDir
  ref <- evalMaybe mayRef
  lift (storeProjectRef name ref)

existingSpec :: ChromatinTest ()
existingSpec = do
  repoDirFP <- tempDir "existing/repo"
  repoDir <- parseAbsDir repoDirFP
  git repoDir ["init", "-q"]
  git repoDir ["config", "user.email", "test@chromatin.hs"]
  git repoDir ["config", "user.name", "chromatin"]
  addFile repoDir [relfile|test|]
  setRef repoDir
  ready <- lift (stackRpluginReady name repoDir False)
  Ready === ready
  addFile repoDir [relfile|test2|]
  incomplete <- lift (stackRpluginReady name repoDir False)
  Incomplete === incomplete

test_existing :: UnitTest
test_existing =
  vars >>= specWithDef existingSpec

noGitSpec :: ChromatinTest ()
noGitSpec = do
  repoDirFP <- tempDir "existing/repo"
  repoDir <- parseAbsDir repoDirFP
  broken <- lift (stackRpluginReady name repoDir False)
  Broken ("not a stack package: " <> (toText repoDirFP) <> "/") === broken

test_noGit :: UnitTest
test_noGit =
  vars >>= specWithDef noGitSpec
