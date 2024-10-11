module HenHen.Utils.Json
( readJson
) where

import Data.Aeson (FromJSON, eitherDecodeStrict)
import Data.ByteString (ByteString)

readJson :: (FromJSON a) => ByteString -> Either String a
readJson = eitherDecodeStrict
