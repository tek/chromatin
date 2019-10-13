module Config where

import Neovim (toObject)

import Ribosome.Test.Embed (Vars(..), varsFromList)
import Ribosome.Test.File (tempDir)
import Ribosome.Test.Unit (uPrefix)

vars :: IO Vars
vars = do
  persistenceDir <- tempDir uPrefix "persist"
  return $ varsFromList [("ribosome_persistence_dir", toObject persistenceDir)]
