{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module HenHen.Config.Target
( Target(..)
, Meta(..)
, MetaKey(..)
, SourceOptions(..)
, EggOptions(..)
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
      Module     Meta SourceOptions
    | Egg        Meta EggOptions
    | Executable Meta SourceOptions

data SourceOptions = SourceOptions
    { sourcePath     :: Maybe FilePath
    , sourceIncludes :: [FilePath] }

newtype EggOptions = EggOptions
    { eggDirectory   :: Maybe FilePath }

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

parseSource :: Object -> Parser SourceOptions
parseSource obj = SourceOptions
    <$> (obj .:? "source")
    <*> optional mempty (obj .:? "includes")

parseEgg :: Object -> Parser EggOptions
parseEgg obj = EggOptions <$> (obj .: "directory")

instance FromJSON Target where
    parseJSON = withObject "Target" $ \obj -> do
        meta <- parseMeta obj
        tag  <- (obj .: "type") :: Parser String
        case map toLower tag of
            "module"     -> Module     meta <$> parseSource obj
            "egg"        -> Egg        meta <$> parseEgg obj
            "executable" -> Executable meta <$> parseSource obj
            _            -> fail $ "unrecognized target tag: " ++ show tag

serializeMeta :: Meta -> [Pair]
serializeMeta meta =
    [ "name"          .= metaKey  meta
    , "dependencies"  .= metaDeps meta
    , "extra-options" .= metaOptions meta ]

instance ToJSON Target where
    toJSON (Module meta options) = object $ serializeMeta meta ++
        [ "tag"       .= ("module" :: String)
        , "source"    .= sourcePath options
        , "includes"  .= sourceIncludes options ]
    toJSON (Egg meta options) = object $ serializeMeta meta ++
        [ "tag"       .= ("egg" :: String)
        , "directory" .= eggDirectory options ]
    toJSON (Executable meta options) = object $ serializeMeta meta ++
        [ "tag"       .= ("executable" :: String)
        , "source"    .= sourcePath options
        , "includes"  .= sourceIncludes options ]
