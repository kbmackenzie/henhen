module HenHen.Utils.String
( stringToByteString
, lazyByteStringToText
, lazyByteStringToString
) where

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text

stringToByteString :: String -> StrictByteString.ByteString
stringToByteString = encodeUtf8 . Text.pack

lazyByteStringToText :: LazyByteString.ByteString -> Text
lazyByteStringToText = decodeUtf8Lenient . LazyByteString.toStrict

lazyByteStringToString :: LazyByteString.ByteString -> String
lazyByteStringToString = Text.unpack . lazyByteStringToText
