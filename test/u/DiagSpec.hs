{-# OPTIONS_GHC -F -pgmF htfpp #-}

module DiagSpec(
  htf_thisModulesTests
) where

import Chromatin.Data.Chromatin (ChromatinN)
import Chromatin.Diag (crmDiag)
import Chromatin.Test.Unit (specWithDef)
import Config (vars)
import Control.Monad.IO.Class (liftIO)
import Ribosome.Api.Buffer (currentBufferContent)
import Test.Framework

target :: [Text]
target = [
  "Diagnostics",
  ""
  ]

diagSpec :: ChromatinN ()
diagSpec = do
  crmDiag
  content <- currentBufferContent
  liftIO $ assertEqual target content

test_diag :: IO ()
test_diag =
  vars >>= specWithDef diagSpec
