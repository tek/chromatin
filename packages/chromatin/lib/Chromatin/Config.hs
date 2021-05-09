module Chromatin.Config where

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
import Chromatin.Data.RpluginSource (FlakeUrl(FlakeUrl), HackageDepspec(HackageDepspec), RpluginSource(..))
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
        "hackage" ->
          Right $ Hackage (HackageDepspec spec')
        "stack" ->
          mapLeft (InvalidPath spec' . show) (Stack <$> parseAbsDir (toString spec'))
        "flake" ->
          Right (Flake (FlakeUrl spec'))
        a ->
          Left (UnknownPrefix spec a)
    source (Right (PlainSpec name')) =
      Right $ Hackage (HackageDepspec name')
    source (Left e) =
      Left (ParseFailure spec e)

rpluginHasName :: RpluginName -> Rplugin -> Bool
rpluginHasName target (Rplugin name _) = target == name

nameFromSource :: RpluginSource -> RpluginName
nameFromSource = \case
  Hackage (HackageDepspec n) ->
    RpluginName n
  Stack fp ->
    RpluginName . toText . toFilePath . dirname $ fp
  Flake (FlakeUrl url) ->
    RpluginName url

modifyExisting :: RpluginSource -> Rplugin -> Bool -> Bool -> RpluginModification
modifyExisting src plug =
  RpluginUpdate plug src

modification ::
  [Rplugin] ->
  RpluginConfig ->
  Either ConfigError RpluginModification
modification current (RpluginConfig spec explicitName dev debug) = do
  source <- sourceFromSpec spec
  let
    name =
      fromMaybe (nameFromSource source) explicitName
    sameName =
      find (rpluginHasName name) current
    cons =
      maybe (RpluginNew name source) (modifyExisting source) sameName
  pure (cons (fromMaybe False dev) (fromMaybe False debug))

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
