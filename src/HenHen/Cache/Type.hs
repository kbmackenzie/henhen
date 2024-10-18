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
import Data.HashSet (HashSet)

data CacheInfo = CacheInfo
    { buildTime     :: Integer
    , dependencySet :: HashSet String }

instance FromJSON CacheInfo where
    parseJSON = withObject "CacheInfo" $ \obj -> CacheInfo
        <$> (obj .: "build-time"    )
        <*> (obj .: "dependency-set")

instance ToJSON CacheInfo where
    toJSON cache = object
        [ "build-time"     .= buildTime cache
        , "dependency-set" .= dependencySet cache ]
