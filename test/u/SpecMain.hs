{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import {-@ HTF_TESTS @-} DiagSpec
import {-@ HTF_TESTS @-} RebuildSpec
import {-@ HTF_TESTS @-} ConfigSpec
import {-@ HTF_TESTS @-} BuildSpec
import {-@ HTF_TESTS @-} RunSpec
import Test.Framework
import Test.Framework.BlackBoxTest ()

main :: IO ()
main = htfMain htf_importedTests
