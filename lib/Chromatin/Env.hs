module Chromatin.Env(
  logError,
) where

import qualified Control.Lens as Lens (over)
import qualified Data.Map.Strict as Map (adjust)
import Ribosome.Api.Exists (epochSeconds)
import qualified Ribosome.Control.Ribo as Ribo (modify)
import Ribosome.Data.Errors (Errors(Errors), Error(Error), ComponentName)
import Chromatin.Data.Chromatin (Chromatin)
import qualified Chromatin.Data.Env as Env (_errors)

storeError :: Int -> ComponentName -> [String] -> Errors -> Errors
storeError time name msg (Errors errors) =
  Errors (Map.adjust (err:) name errors)
  where
    err = Error time msg

logError :: ComponentName -> String -> Chromatin ()
logError name e = do
  time <- epochSeconds
  Ribo.modify $ Lens.over Env._errors (storeError time name [e])
