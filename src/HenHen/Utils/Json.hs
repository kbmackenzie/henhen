module HenHen.Utils.Json
( readJson
, writeJson
) where

import Data.Aeson (FromJSON, ToJSON, encode, eitherDecodeStrict)
import Data.ByteString (ByteString, toStrict)

readJson :: (FromJSON a) => ByteString -> Either String a
readJson = eitherDecodeStrict

writeJson :: (ToJSON a) => a -> ByteString
writeJson = toStrict . encode
