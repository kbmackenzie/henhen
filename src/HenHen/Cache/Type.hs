{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module HenHen.Cache.Type
( CacheData(..)
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

data CacheData = CacheData
    { buildTime :: Integer
    , fileMap   :: HashMap FilePath Integer }

instance FromJSON CacheData where
    parseJSON = withObject "CacheData" $ \obj -> CacheData
        <$> (obj .: "build-time")
        <*> (obj .: "file-map")

instance ToJSON CacheData where
    toJSON cache = object
        [ "build-time" .= buildTime cache
        , "file-map"   .= fileMap cache ]
