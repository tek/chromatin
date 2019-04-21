module Chromatin.Diag where

import Data.Functor (void)
import Ribosome.Data.ScratchOptions (defaultScratchOptions, scratchFocus)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Scratch (showInScratch)

diagnostics ::
  Monad m =>
  m [Text]
diagnostics = return ["Diagnostics", ""]

crmDiag ::
  MonadRibo m =>
  NvimE e m =>
  MonadDeepError e DecodeError m =>
  m ()
crmDiag = do
  content <- diagnostics
  void $ showInScratch content (scratchFocus $ defaultScratchOptions "chromatin-diagnostics")
