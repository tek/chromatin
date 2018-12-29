module Chromatin.Test.Unit(
  spec,
  specWith,
  specWithDef,
  specConfig,
) where

import Data.Default.Class (def)
import UnliftIO.STM (newTVarIO)
import Ribosome.Test.Embed (Vars, TestConfig)
import Ribosome.Test.Unit (unitSpec)
import Chromatin.Data.Chromatin (Chromatin)
import Chromatin.Data.Env (Env)
import Chromatin.Test.Config (defaultTestConfig, defaultTestConfigWith)

specConfig :: TestConfig -> Env -> Chromatin () -> IO ()
specConfig conf e s = do
  t <- newTVarIO e
  unitSpec conf t s

spec :: Env -> Chromatin () -> IO ()
spec =
  specConfig defaultTestConfig

specWith :: Env -> Chromatin () -> Vars -> IO ()
specWith e s vars = do
  t <- newTVarIO e
  unitSpec (defaultTestConfigWith vars) t s

specWithDef :: Chromatin () -> Vars -> IO ()
specWithDef =
  specWith def
