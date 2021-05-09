module Chromatin.Test.Config where

import Neovim (toObject)
import Ribosome.Test.Embed (TestConfig, Vars, varsFromList)
import qualified Ribosome.Test.Embed as E (defaultTestConfig, defaultTestConfigWith)
import Ribosome.Test.File (tempDir)
import Ribosome.Test.Unit (uPrefix)

defaultTestConfigWith :: Vars -> TestConfig
defaultTestConfigWith = E.defaultTestConfigWith "chromatin"

defaultTestConfig :: TestConfig
defaultTestConfig = E.defaultTestConfig "chromatin"

vars ::
  MonadIO m =>
  m Vars
vars = do
  persistenceDir <- tempDir uPrefix "persist"
  return $ varsFromList [("ribosome_persistence_dir", toObject persistenceDir)]
