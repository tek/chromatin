module Chromatin.Test.Unit where

import Chromatin.Data.Chromatin (ChromatinN)
import Chromatin.Data.Env (Env)
import Chromatin.Test.Config (defaultTestConfig, defaultTestConfigWith)
import Data.Default (def)
import Ribosome.Test.Embed (TestConfig, Vars)
import Ribosome.Test.Unit (unitSpec)

specConfig :: TestConfig -> Env -> ChromatinN () -> IO ()
specConfig =
  unitSpec

spec :: Env -> ChromatinN () -> IO ()
spec =
  specConfig defaultTestConfig

specWith :: Env -> ChromatinN () -> Vars -> IO ()
specWith e s vars =
  unitSpec (defaultTestConfigWith vars) e s

specWithDef :: ChromatinN () -> Vars -> IO ()
specWithDef =
  specWith def
