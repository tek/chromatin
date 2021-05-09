module Chromatin.Diag where

import Ribosome.Data.ScratchOptions (defaultScratchOptions, scratchFocus)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Scratch (showInScratch)

diagnostics ::
  Monad m =>
  m [Text]
diagnostics = return ["Diagnostics", ""]

crmDiag ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  m ()
crmDiag = do
  content <- diagnostics
  void $ showInScratch content (scratchFocus $ defaultScratchOptions "chromatin-diagnostics")
