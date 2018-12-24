{-# OPTIONS_GHC -F -pgmF htfpp #-}

module DiagSpec(
  htf_thisModulesTests
) where

import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (Default(def))
import Test.Framework
import Ribosome.Api.Buffer (currentBufferContent)
import Chromatin.Data.Chromatin
import Chromatin.Test.Unit (specWithDef)
import Chromatin.Diag (crmDiag)
import Config (vars)

target :: [String]
target = [
  "Diagnostics",
  ""
  ]

diagSpec :: Chromatin ()
diagSpec = do
  crmDiag def
  content <- currentBufferContent
  liftIO $ assertEqual target content

test_diag :: IO ()
test_diag =
  vars >>= specWithDef diagSpec
