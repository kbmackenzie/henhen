{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module HenHen.Cache.Type
( CacheInfo(..)
) where

import Data.Aeson
    ( FromJSON(..)
    , ToJSON(..)
    , withObject
    , object
    , (.:)
    , (.=)
    )
import Data.HashMap.Strict (HashMap)

data CacheInfo = CacheInfo
    { buildTime     :: Integer
    , dependencyMap :: HashMap String Integer }

instance FromJSON CacheInfo where
    parseJSON = withObject "CacheInfo" $ \obj -> CacheInfo
        <$> (obj .: "build-time")
        <*> (obj .: "dependency-map")

instance ToJSON CacheInfo where
    toJSON cache = object
        [ "build-time"     .= buildTime cache
        , "dependency-map" .= dependencyMap cache ]
