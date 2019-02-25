module Chromatin.Data.String(
  capitalize,
) where

import Data.Char (toUpper)

capitalize :: String -> String
capitalize [] = []
capitalize (head' : tail') = toUpper head' : tail'
