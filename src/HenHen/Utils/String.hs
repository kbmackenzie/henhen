module HenHen.Utils.String
( lazyByteStringToText
, lazyByteStringToString
) where

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text

lazyByteStringToText :: LazyByteString.ByteString -> Text
lazyByteStringToText = decodeUtf8Lenient . LazyByteString.toStrict

lazyByteStringToString :: LazyByteString.ByteString -> String
lazyByteStringToString = Text.unpack . lazyByteStringToText
