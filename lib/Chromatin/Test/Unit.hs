module Chromatin.Test.Unit where

import Chromatin.Data.Chromatin (Chromatin)
import Chromatin.Data.Env (Env)
import Chromatin.Test.Config (defaultTestConfig, defaultTestConfigWith)
import Data.Default (def)
import Ribosome.Test.Embed (TestConfig, Vars)
import Ribosome.Test.Unit (unitSpec)

specConfig :: TestConfig -> Env -> Chromatin () -> IO ()
specConfig =
  unitSpec

spec :: Env -> Chromatin () -> IO ()
spec =
  specConfig defaultTestConfig

specWith :: Env -> Chromatin () -> Vars -> IO ()
specWith e s vars =
  unitSpec (defaultTestConfigWith vars) e s

specWithDef :: Chromatin () -> Vars -> IO ()
specWithDef =
  specWith def
