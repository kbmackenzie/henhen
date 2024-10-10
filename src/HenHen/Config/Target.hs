{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HenHen.Config.Target
( Target(..)
, Meta(..)
, MetaKey(..)
, SourceOptions(..)
, EggOptions(..)
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
    , metaOptions :: [String]  }

newtype MetaKey = MetaKey { getKey :: String }
    deriving (Eq, Ord, Hashable)

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
-- Utilities:
------------------------------------
getTargetMeta :: Target -> Meta
getTargetMeta (Module meta _) = meta
getTargetMeta (Egg meta _) = meta
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
