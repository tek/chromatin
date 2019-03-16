module Chromatin.Config(
  readConfig,
  analyzeConfig,
  RpluginModification(..),
  analyzeConfigIO,
) where

import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Ribosome.Config.Setting (SettingError, settingR)
import qualified Ribosome.Control.Ribo as Ribo (inspect)
import System.FilePath (takeFileName)
import Text.ParserCombinators.Parsec

import Chromatin.Data.Chromatin (Chromatin, ChromatinE)
import qualified Chromatin.Data.Env as Env (rplugins)
import Chromatin.Data.Rplugin (Rplugin(Rplugin))
import Chromatin.Data.RpluginConfig (RpluginConfig(RpluginConfig))
import Chromatin.Data.RpluginName (RpluginName(RpluginName))
import Chromatin.Data.RpluginSource (HackageDepspec(HackageDepspec), PypiDepspec(PypiDepspec), RpluginSource(..))
import Chromatin.Data.Rplugins (Rplugins(Rplugins))
import qualified Chromatin.Settings as S (rplugins)

readConfig :: ChromatinE SettingError Rplugins
readConfig = Rplugins <$> settingR S.rplugins

data RpluginModification =
  RpluginNew RpluginName RpluginSource Bool Bool
  |
  RpluginRemove Rplugin
  |
  RpluginUpdate Rplugin RpluginSource Bool Bool
  |
  RpluginKeep Rplugin
  deriving (Eq, Show)

data ParsedSpec =
  PrefixedSpec String String
  |
  PlainSpec String

parsePrefixed :: GenParser Char st ParsedSpec
parsePrefixed = do
  prefix <- many $ noneOf ":"
  _ <- char ':'
  spec <- many anyChar
  return $ PrefixedSpec prefix spec

specParser :: GenParser Char st ParsedSpec
specParser = try parsePrefixed <|> fmap PlainSpec (many anyChar)

parseSpec :: String -> Either ParseError ParsedSpec
parseSpec = parse specParser "none"

sourceFromSpec :: String -> Either String RpluginSource
sourceFromSpec spec =
  case parseSpec spec of
    Right (PrefixedSpec prefix spec') -> case prefix of
      "pip" -> Right $ Pypi (PypiDepspec spec')
      "hackage" -> Right $ Hackage (HackageDepspec spec')
      "stack" -> Right $ Stack spec'
      a -> Left $ "unknown rplugin prefix `" ++ a ++ "` in `" ++ spec ++ "`"
    Right (PlainSpec name') ->
      Right $ Hackage (HackageDepspec name')
    Left _ -> Left $ "failed to parse `" ++ spec ++ "`"

rpluginHasName :: RpluginName -> Rplugin -> Bool
rpluginHasName target (Rplugin name _) = target == name

nameFromSource :: RpluginSource -> RpluginName
nameFromSource (Hackage (HackageDepspec n)) = RpluginName n
nameFromSource (Stack fp) = RpluginName (takeFileName fp)
nameFromSource (Pypi (PypiDepspec n)) = RpluginName n

modifyExisting :: RpluginSource -> Rplugin -> RpluginModification
modifyExisting _ _ = undefined

modification :: [Rplugin] -> RpluginConfig -> Either String RpluginModification
modification current (RpluginConfig spec explicitName dev debug) = do
  source <- sourceFromSpec spec
  let name = fromMaybe (nameFromSource source) explicitName
  let sameName = find (rpluginHasName name) current
  return $ maybe (RpluginNew name source (fromMaybe False dev) (fromMaybe False debug)) (modifyExisting source) sameName

removals :: [RpluginModification] -> [RpluginModification]
removals _ = []

analyzeConfig :: [Rplugin] -> Rplugins -> Either String [RpluginModification]
analyzeConfig current (Rplugins rplugins) = do
  let mods = traverse (modification current) rplugins
  fmap (\m -> m ++ removals m) mods

analyzeConfigIO :: Rplugins -> Chromatin (Either String [RpluginModification])
analyzeConfigIO rplugins = do
  current <- Ribo.inspect Env.rplugins
  return $ analyzeConfig current rplugins
