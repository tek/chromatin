{-# LANGUAGE TemplateHaskell #-}

module Chromatin.Data.Env(
  Env,
) where

import Control.Lens (makeClassy_)
import Data.Default.Class (Default(def))
import Ribosome.Data.Errors (Errors)

data Env =
  Env {
    rplugins :: [String],
    errors :: Errors
  }
  deriving Show

makeClassy_ ''Env

instance Default Env where
  def = Env def def
