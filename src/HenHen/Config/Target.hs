{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HenHen.Config.Target
( Target(..)
, Meta(..)
, MetaKey(..)
, EggOptions(..)
, SourceOptions(..)
, getTargetMeta
, getTargetKey
, getTargetMap
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
import Data.Char (toLower, isAlphaNum)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

data Meta = Meta
    { metaKey     :: MetaKey
    , metaDeps    :: [MetaKey]
    , metaTrack   :: [FilePath]
    , metaOptions :: [String]   }

newtype MetaKey = MetaKey { getKey :: String }
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

getTargetKey :: Target -> MetaKey
getTargetKey = metaKey . getTargetMeta

getTargetMap :: [Target] -> HashMap MetaKey Target
getTargetMap = HashMap.fromList . map toPair
    where toPair target = (getTargetKey target, target)

------------------------------------
-- JSON/YAML parsing:
------------------------------------

instance FromJSON MetaKey where
    parseJSON = withText "MetaKey" $
        \text -> if Text.all isAlphaNum text
            then (return . MetaKey . Text.unpack) text
            else fail ("Invalid target key: " ++ show text)

instance ToJSON MetaKey where
    toJSON = toJSON . getKey

parseMeta :: Object -> Parser Meta
parseMeta obj = Meta
    <$> (obj .: "name")
    <*> optional mempty (obj .:? "dependencies")
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
    [ "name"          .= metaKey  meta
    , "dependencies"  .= metaDeps meta
    , "track"         .= metaTrack meta
    , "extra-options" .= metaOptions meta ]

instance ToJSON Target where
    toJSON (Egg meta options) = object $ serializeMeta meta ++
        [ "tag"       .= ("egg" :: String)
        , "directory" .= eggDirectory options ]
    toJSON (Executable meta options) = object $ serializeMeta meta ++
        [ "tag"       .= ("executable" :: String)
        , "source"    .= sourcePath options   ]
