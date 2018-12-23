{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Test.Framework
import Test.Framework.BlackBoxTest ()

main :: IO ()
main = do
  htfMain htf_importedTests
