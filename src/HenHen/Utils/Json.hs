module HenHen.Utils.Json
( readJson
, writeJson
) where

import Data.Aeson (FromJSON, ToJSON, encode, eitherDecodeStrict)
import Data.ByteString (ByteString, toStrict)
import Data.Bifunctor (first)

readJson :: (FromJSON a) => ByteString -> Either String a
readJson = first context . eitherDecodeStrict
    where context = ("couldn't parse json: " ++)

writeJson :: (ToJSON a) => a -> ByteString
writeJson = toStrict . encode
