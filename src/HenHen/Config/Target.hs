{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module HenHen.Config.Target
( Target(..)
, Meta(..)
, MetaKey(..)
, ModuleOptions(..)
, EggOptions(..)
, ExecutableOptions(..)
) where

import Data.Aeson
    ( ToJSON(..)
    , FromJSON(..)
    , Value
    , Object
    , (.:)
    , (.:?)
    , withObject
    , withText
    )
import Data.Aeson.Types (Parser)
import HenHen.Utils.Maybe (optional)
import qualified Data.Text as Text
import Data.Char (toLower)

data Meta = Meta
    { metaKey   :: MetaKey
    , metaDeps  :: [MetaKey] }

newtype MetaKey = MetaKey { getKey :: String }

data Target =
      Module     Meta ModuleOptions
    | Egg        Meta EggOptions
    | Executable Meta ExecutableOptions

data ModuleOptions = ModuleOptions
    { moduleSource   :: FilePath
    , moduleIncludes :: [FilePath] }

newtype EggOptions = EggOptions
    { eggDirectory   :: Maybe FilePath }

data ExecutableOptions = ExecutableOptions
    { executableName     :: String
    , executableSource   :: FilePath
    , executableStatic   :: Bool
    , executableIncludes :: [FilePath] }

------------------------------------
-- JSON/YAML parsing:
------------------------------------

instance FromJSON MetaKey where
    parseJSON = withText "MetaKey" (return . MetaKey . Text.unpack)

instance ToJSON MetaKey where
    toJSON = toJSON . getKey

parseMeta :: Object -> Parser Meta
parseMeta obj = Meta
    <$> (obj .: "name")
    <*> (obj .: "dependencies")

parseModule :: Object -> Parser ModuleOptions
parseModule obj = ModuleOptions
    <$> (obj .: "source")
    <*> optional mempty (obj .:? "includes")

parseEgg :: Object -> Parser EggOptions
parseEgg obj = EggOptions <$> (obj .: "directory")

parseExecutable :: Object -> Parser ExecutableOptions
parseExecutable obj = ExecutableOptions
    <$> (obj .: "name")
    <*> (obj .: "source")
    <*> optional True (obj .:? "static")
    <*> optional mempty (obj .:? "includes")

instance FromJSON Target where
    parseJSON = withObject "Target" $ \obj -> do
        meta <- parseMeta obj
        tag  <- (obj .: "type") :: Parser String
        case map toLower tag of
            "module"     -> Module     meta <$> parseModule obj
            "egg"        -> Egg        meta <$> parseEgg obj
            "executable" -> Executable meta <$> parseExecutable obj
            _            -> fail $ "unrecognized target tag: " ++ show tag
