module Chromatin.Config where

import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Path (dirname, parseAbsDir, toFilePath)
import Ribosome.Config.Setting (setting)
import Ribosome.Data.SettingError (SettingError)
import Text.ParserCombinators.Parsec (GenParser, ParseError, anyChar, char, noneOf, parse, try)

import Chromatin.Data.ConfigError (ConfigError(..))
import Chromatin.Data.Env (Env)
import qualified Chromatin.Data.Env as Env (rplugins)
import Chromatin.Data.Rplugin (Rplugin(Rplugin))
import Chromatin.Data.RpluginConfig (RpluginConfig(RpluginConfig))
import Chromatin.Data.RpluginName (RpluginName(RpluginName))
import Chromatin.Data.RpluginSource (HackageDepspec(HackageDepspec), PypiDepspec(PypiDepspec), RpluginSource(..))
import Chromatin.Data.Rplugins (Rplugins(Rplugins))
import qualified Chromatin.Settings as S (rplugins)

readConfig ::
  MonadRibo m =>
  NvimE e m =>
  MonadDeepError e SettingError m =>
  m Rplugins
readConfig =
  Rplugins <$> setting S.rplugins

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
  PrefixedSpec Text Text
  |
  PlainSpec Text
  deriving (Eq, Show)

parsePrefixed :: GenParser Char st ParsedSpec
parsePrefixed = do
  prefix <- many $ noneOf ":"
  _ <- char ':'
  spec <- many anyChar
  return $ PrefixedSpec (toText prefix) (toText spec)

specParser :: GenParser Char st ParsedSpec
specParser = try parsePrefixed <|> fmap (PlainSpec . toText) (many anyChar)

parseSpec :: Text -> Either ParseError ParsedSpec
parseSpec =
  parse specParser "none" . toString

sourceFromSpec :: Text -> Either ConfigError RpluginSource
sourceFromSpec spec =
  source . parseSpec $ spec
  where
    source (Right (PrefixedSpec prefix spec')) =
      case prefix of
        "pip" ->
          Right $ Pypi (PypiDepspec spec')
        "hackage" ->
          Right $ Hackage (HackageDepspec spec')
        "stack" ->
          mapLeft (InvalidPath spec' . show) (Stack <$> parseAbsDir (toString spec'))
        a -> Left (UnknownPrefix spec a)
    source (Right (PlainSpec name')) =
      Right $ Hackage (HackageDepspec name')
    source (Left e) =
      Left (ParseFailure spec e)

rpluginHasName :: RpluginName -> Rplugin -> Bool
rpluginHasName target (Rplugin name _) = target == name

nameFromSource :: RpluginSource -> RpluginName
nameFromSource (Hackage (HackageDepspec n)) = RpluginName n
nameFromSource (Stack fp) = RpluginName . toText . toFilePath . dirname $ fp
nameFromSource (Pypi (PypiDepspec n)) = RpluginName n

modifyExisting :: RpluginSource -> Rplugin -> RpluginModification
modifyExisting _ _ = undefined

modification ::
  [Rplugin] ->
  RpluginConfig ->
  Either ConfigError RpluginModification
modification current (RpluginConfig spec explicitName dev debug) = do
  source <- sourceFromSpec spec
  let name = fromMaybe (nameFromSource source) explicitName
  let sameName = find (rpluginHasName name) current
  return $ maybe (RpluginNew name source (fromMaybe False dev) (fromMaybe False debug)) (modifyExisting source) sameName

removals :: [RpluginModification] -> [RpluginModification]
removals _ = []

analyzeConfig ::
  [Rplugin] ->
  Rplugins ->
  Either ConfigError [RpluginModification]
analyzeConfig current (Rplugins rplugins) = do
  let mods = traverse (modification current) rplugins
  fmap (\m -> m <> removals m) mods

analyzeConfigIO ::
  MonadDeepState s Env m =>
  MonadDeepError e ConfigError m =>
  Rplugins ->
  m [RpluginModification]
analyzeConfigIO rplugins = do
  current <- getL @Env Env.rplugins
  hoistEither $ analyzeConfig current rplugins
