module Chromatin.Test.DiagTest where

import Hedgehog ((===))
import Ribosome.Api.Buffer (currentBufferContent)
import Ribosome.Test.Run (UnitTest)

import Chromatin.Diag (crmDiag)
import Chromatin.Test.Config (vars)
import Chromatin.Test.Unit (ChromatinTest, specWithDef)

target :: [Text]
target = [
  "Diagnostics",
  ""
  ]

diagSpec :: ChromatinTest ()
diagSpec = do
  lift crmDiag
  content <- lift currentBufferContent
  target === content

test_diag :: UnitTest
test_diag =
  vars >>= specWithDef diagSpec
