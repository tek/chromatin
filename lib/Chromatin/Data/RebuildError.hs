module Chromatin.Data.RebuildError(
  RebuildError(..),
) where

import System.Log (Priority(ERROR))

import Ribosome.Data.ErrorReport (ErrorReport(..))
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Error.Report.Class (ReportError(..))

data RebuildError =
  Setting SettingError
  |
  Analysis Text
  deriving Show

instance ReportError RebuildError where
  errorReport (Setting err) = errorReport err
  errorReport (Analysis message) =
    ErrorReport message ["rplugin config analysis error:", message] ERROR
