{-# LANGUAGE TemplateHaskell #-}

module Chromatin.Data.Error where

import Data.DeepPrisms (deepPrisms)
import Prelude hiding (Decode)
import Ribosome.Data.Mapping (MappingError)
import Ribosome.Data.PersistError (PersistError)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Error.Report.Class (ReportError(..))
import Ribosome.Msgpack.Error (DecodeError)

import Chromatin.Data.ConfigError (ConfigError)

data Error =
  Rpc RpcError
  |
  Mapping MappingError
  |
  Decode DecodeError
  |
  Persist PersistError
  |
  Setting SettingError
  |
  Config ConfigError
  deriving Show

deepPrisms ''Error

instance ReportError Error where
  errorReport (Rpc e) = errorReport e
  errorReport (Mapping e) = errorReport e
  errorReport (Decode e) = errorReport e
  errorReport (Persist e) = errorReport e
  errorReport (Setting e) = errorReport e
  errorReport (Config e) = errorReport e
