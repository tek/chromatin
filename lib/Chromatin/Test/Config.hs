module Chromatin.Test.Config(
  defaultTestConfigWith,
  defaultTestConfig,
) where

import Ribosome.Test.Embed (TestConfig, Vars)
import qualified Ribosome.Test.Embed as E (defaultTestConfig, defaultTestConfigWith)

defaultTestConfigWith :: Vars -> TestConfig
defaultTestConfigWith = E.defaultTestConfigWith "chromatin"

defaultTestConfig :: TestConfig
defaultTestConfig = E.defaultTestConfig "chromatin"
