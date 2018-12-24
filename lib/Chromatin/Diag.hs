module Chromatin.Diag(
  crmDiag,
) where

import Data.Functor (void)
import Neovim (CommandArguments)
import Ribosome.Data.ScratchOptions (defaultScratchOptions)
import Ribosome.Scratch (showInScratch)
import Chromatin.Data.Chromatin (Chromatin)

diagnostics :: Chromatin [String]
diagnostics = return ["Diagnostics", ""]

crmDiag :: CommandArguments -> Chromatin ()
crmDiag _ = do
  content <- diagnostics
  void $ showInScratch content (defaultScratchOptions "chromatin-diagnostics")
