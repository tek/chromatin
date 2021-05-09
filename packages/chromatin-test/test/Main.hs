module Main where

import Chromatin.Test.ConfigTest (test_analyzeConfig)
import Chromatin.Test.DiagTest (test_diag)
import Chromatin.Test.ExistingTest (test_existing, test_noGit)
import Chromatin.Test.NixTest (test_nix)
import Chromatin.Test.RebuildTest (test_rebuild)
import Ribosome.Test.Run (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "all" [
    unitTest "config" test_analyzeConfig,
    unitTest "diag" test_diag,
    unitTest "existing" test_existing,
    unitTest "no git" test_noGit,
    unitTest "rebuild" test_rebuild,
    -- unitTest "run" test_run,
    unitTest "nix" test_nix
    -- unitTest "stack" test_stack
  ]

main :: IO ()
main =
  defaultMain tests
