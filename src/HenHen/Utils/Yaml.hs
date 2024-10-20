module HenHen.Utils.Yaml
( readYaml
, prettyYaml
) where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Yaml (decodeEither', FromJSON, ToJSON, prettyPrintParseException)
import Data.Yaml.Pretty (encodePretty, setConfCompare, defConfig)
import Data.Bifunctor (first)

readYaml :: (FromJSON a) => ByteString -> Either String a
readYaml = first (context . prettyPrintParseException) . decodeEither'
    where context = ("couldn't parse yaml: " ++)

prettyYaml :: (ToJSON a) => (Text -> Text -> Ordering) -> a -> ByteString
prettyYaml = encodePretty . flip setConfCompare defConfig
