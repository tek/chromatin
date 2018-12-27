module Chromatin.Log(
  debug,
  info,
  debugS,
  infoS,
  R.p,
) where

import Ribosome.Data.Ribo (Ribo)
import qualified Ribosome.Log as R (debug, info, p)

debug :: String -> Ribo e ()
debug = R.debug "chromatin"

info :: String -> Ribo e ()
info = R.info "chromatin"

debugS :: Show a => a -> Ribo e ()
debugS = R.debug "chromatin"

infoS :: Show a => a -> Ribo e ()
infoS = R.info "chromatin"
