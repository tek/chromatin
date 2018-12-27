module Chromatin.Config(
  readConfig,
  analyzeConfig,
  RpluginModification(..),
  analyzeConfigIO,
) where

import Data.Maybe (fromMaybe)
import Data.Foldable (find)
import Text.ParserCombinators.Parsec
import qualified Ribosome.Data.Ribo as Ribo (inspect)
import Ribosome.Config.Setting (setting)
import Chromatin.Data.Chromatin (Chromatin)
import qualified Chromatin.Data.Env as Env (rplugins)
import Chromatin.Data.Rplugin (Rplugin(Rplugin))
import Chromatin.Data.RpluginName (RpluginName(RpluginName))
import Chromatin.Data.RpluginSource (RpluginSource(..), HackageDepspec(HackageDepspec), PypiDepspec(PypiDepspec))
import Chromatin.Data.Rplugins (Rplugins(Rplugins))
import Chromatin.Data.RpluginConfig (RpluginConfig(RpluginConfig))
import qualified Chromatin.Settings as S (rplugins)

readConfig :: Chromatin Rplugins
readConfig = setting S.rplugins

data RpluginModification =
  RpluginNew RpluginName RpluginSource
  |
  RpluginRemove Rplugin
  |
  RpluginUpdate Rplugin RpluginSource
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
    Right (PrefixedSpec prefix name') -> case prefix of
      "pip" -> Right $ Pypi (PypiDepspec name')
      "hackage" -> Right $ Hackage (HackageDepspec name')
      a -> Left $ "unknown rplugin prefix `" ++ a ++ "` in `" ++ spec ++ "`"
    Right (PlainSpec name') ->
      Right $ Hackage (HackageDepspec name')
    Left _ -> Left $ "failed to parse `" ++ spec ++ "`"

rpluginHasName :: RpluginName -> Rplugin -> Bool
rpluginHasName target (Rplugin name _) = target == name

nameFromSource :: RpluginSource -> RpluginName
nameFromSource (Hackage (HackageDepspec n)) = RpluginName n
nameFromSource (Pypi (PypiDepspec n)) = RpluginName n

modifyExisting :: RpluginSource -> Rplugin -> RpluginModification
modifyExisting _ _ = undefined

modification :: [Rplugin] -> RpluginConfig -> Either String RpluginModification
modification current (RpluginConfig spec explicitName) = do
  source <- sourceFromSpec spec
  let name = fromMaybe (nameFromSource source) explicitName
  let sameName = find (rpluginHasName name) current
  return $ maybe (RpluginNew name source) (modifyExisting source) sameName

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
