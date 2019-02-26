module Chromatin.Data.RebuildError(
  RebuildError(..),
) where

import System.Log (Priority(ERROR))

import Ribosome.Config.Setting (SettingError)
import Ribosome.Data.ErrorReport (ErrorReport(..))
import Ribosome.Error.Report (ReportError(..))

data RebuildError =
  Setting SettingError
  |
  Analysis String
  deriving Show

instance ReportError RebuildError where
  errorReport (Setting err) = errorReport err
  errorReport (Analysis message) =
    ErrorReport message ["rplugin config analysis error:", message] ERROR
