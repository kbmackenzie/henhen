{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HenHen.Config.Target
( Target(..)
, Meta(..)
, TargetKey(..)
, EggOptions(..)
, SourceOptions(..)
, getTargetMeta
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
    , FromJSONKey(..)
    , ToJSONKey(..)
    , FromJSONKeyFunction(..)
    )
import Data.Aeson.Types (Parser, Pair, toJSONKeyText)
import HenHen.Utils.Maybe (optional)
import Data.Text (Text)
import Data.Char (toLower, isAlphaNum)
import Data.Hashable (Hashable)
import qualified Data.Text as Text

data Meta = Meta
    { metaDeps    :: [TargetKey]
    , metaTrack   :: [FilePath]
    , metaOptions :: [String]   }

newtype TargetKey = TargetKey { getKey :: String }
    deriving (Eq, Ord, Hashable)

data Target =
      Egg        Meta EggOptions
    | Executable Meta SourceOptions

newtype SourceOptions = SourceOptions
    { sourcePath     :: Maybe FilePath }

newtype EggOptions = EggOptions
    { eggDirectory   :: Maybe FilePath }

------------------------------------
-- Utilities:
------------------------------------
getTargetMeta :: Target -> Meta
getTargetMeta (Egg meta _)        = meta
getTargetMeta (Executable meta _) = meta

------------------------------------
-- JSON/YAML parsing:
------------------------------------

parseTargetKey :: Text -> Parser TargetKey
parseTargetKey text = if Text.all isAlphaNum text
    then (return . TargetKey . Text.unpack) text
    else fail ("invalid character in key for target: " ++ show text)

instance FromJSON TargetKey where
    parseJSON = withText "TargetKey" parseTargetKey

instance ToJSON TargetKey where
    toJSON = toJSON . getKey

instance FromJSONKey TargetKey where
    fromJSONKey = FromJSONKeyTextParser parseTargetKey

instance ToJSONKey TargetKey where
    toJSONKey = toJSONKeyText (Text.pack . getKey)

parseMeta :: Object -> Parser Meta
parseMeta obj = Meta
    <$> optional mempty (obj .:? "dependencies")
    <*> optional mempty (obj .:? "track")
    <*> optional mempty (obj .:? "extra-options")

parseSource :: Object -> Parser SourceOptions
parseSource obj = SourceOptions <$> (obj .:? "source")

parseEgg :: Object -> Parser EggOptions
parseEgg obj = EggOptions <$> (obj .: "directory")

instance FromJSON Target where
    parseJSON = withObject "Target" $ \obj -> do
        meta <- parseMeta obj
        tag  <- (obj .: "type") :: Parser String
        case map toLower tag of
            "egg"        -> Egg        meta <$> parseEgg obj
            "executable" -> Executable meta <$> parseSource obj
            _            -> fail $ "unrecognized target tag: " ++ show tag

serializeMeta :: Meta -> [Pair]
serializeMeta meta =
    [ "dependencies"  .= metaDeps meta
    , "track"         .= metaTrack meta
    , "extra-options" .= metaOptions meta ]

instance ToJSON Target where
    toJSON (Egg meta options) = object $ serializeMeta meta ++
        [ "type"      .= ("egg" :: String)
        , "directory" .= eggDirectory options ]
    toJSON (Executable meta options) = object $ serializeMeta meta ++
        [ "type"      .= ("executable" :: String)
        , "source"    .= sourcePath options   ]
