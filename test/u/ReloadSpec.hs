{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ReloadSpec(
  htf_thisModulesTests,
) where

import Chromatin.Data.Chromatin (ChromatinN)
import Chromatin.Test.Unit (specWithDef)
import Config (vars)
import Control.Monad.IO.Class (liftIO)
import Test.Framework

reloadSpec :: ChromatinN ()
reloadSpec =
  liftIO $ assertEqual ("" :: Text) ""

test_reload :: IO ()
test_reload =
  vars >>= specWithDef reloadSpec
