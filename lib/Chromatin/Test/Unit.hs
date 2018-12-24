module Chromatin.Test.Unit(
  spec,
  specWith,
  specWithDef,
) where

import Data.Default.Class (def)
import UnliftIO.STM (newTVarIO)
import Ribosome.Test.Embed (Vars)
import Ribosome.Test.Unit (unitSpec)
import Chromatin.Data.Chromatin (Chromatin)
import Chromatin.Data.Env (Env)
import Chromatin.Test.Config (defaultTestConfig, defaultTestConfigWith)

spec :: Env -> Chromatin () -> IO ()
spec e s = do
  t <- newTVarIO e
  unitSpec defaultTestConfig t s

specWith :: Env -> Chromatin () -> Vars -> IO ()
specWith e s vars = do
  t <- newTVarIO e
  unitSpec (defaultTestConfigWith vars) t s

specWithDef :: Chromatin () -> Vars -> IO ()
specWithDef s v =
  specWith def s v
