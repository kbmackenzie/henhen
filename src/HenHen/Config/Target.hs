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
    , Object
    , (.:)
    , (.:?)
    , withObject
    , withText
    , object
    , (.=)
    )
import Data.Aeson.Types (Parser, Pair)
import HenHen.Utils.Maybe (optional)
import qualified Data.Text as Text
import Data.Char (toLower)

data Meta = Meta
    { metaKey     :: MetaKey
    , metaDeps    :: [MetaKey]
    , metaOptions :: [String]  }

newtype MetaKey = MetaKey { getKey :: String }

data Target =
      Module     Meta ModuleOptions
    | Egg        Meta EggOptions
    | Executable Meta ExecutableOptions

data ModuleOptions = ModuleOptions
    { moduleSource   :: Maybe FilePath
    , moduleIncludes :: [FilePath]     }

newtype EggOptions = EggOptions
    { eggDirectory   :: Maybe FilePath }

data ExecutableOptions = ExecutableOptions
    { executableOutput   :: Maybe FilePath
    , executableSource   :: Maybe FilePath
    , executableIncludes :: [FilePath]     }

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
    <*> optional mempty (obj .:? "dependencies")
    <*> optional mempty (obj .:? "extra-options")

parseModule :: Object -> Parser ModuleOptions
parseModule obj = ModuleOptions
    <$> (obj .:? "source")
    <*> optional mempty (obj .:? "includes")

parseEgg :: Object -> Parser EggOptions
parseEgg obj = EggOptions <$> (obj .: "directory")

parseExecutable :: Object -> Parser ExecutableOptions
parseExecutable obj = ExecutableOptions
    <$> (obj .:? "output")
    <*> (obj .:? "source")
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

serializeMeta :: Meta -> [Pair]
serializeMeta meta =
    [ "name"          .= metaKey  meta
    , "dependencies"  .= metaDeps meta
    , "extra-options" .= metaOptions meta ]

instance ToJSON Target where
    toJSON (Module meta options) = object $ serializeMeta meta ++
        [ "tag"       .= ("module" :: String)
        , "source"    .= moduleSource options
        , "includes"  .= moduleIncludes options ]
    toJSON (Egg meta options) = object $ serializeMeta meta ++
        [ "tag"       .= ("egg" :: String)
        , "directory" .= eggDirectory options ]
    toJSON (Executable meta options) = object $ serializeMeta meta ++
        [ "tag"       .= ("exeuctable" :: String)
        , "output"    .= executableOutput options
        , "source"    .= executableSource options
        , "includes"  .= executableIncludes options ]
