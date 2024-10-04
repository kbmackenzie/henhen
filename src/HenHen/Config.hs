{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module HenHen.Config
( HenHenConfig(..)
, Aliases(..)
, configFieldOrder
) where

import Data.Aeson
    ( ToJSON(..)
    , FromJSON(..)
    , withObject
    , (.=)
    , (.:?)
    , object
    )
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Function (on)
import Data.Maybe (fromMaybe)

data HenHenConfig = HenHenConfig
    { chickenDeps  :: [Text]
    , specialDeps  :: HashMap Text Text
    , aliases      :: Maybe Aliases     }
    deriving (Show)

data Aliases = Aliases
    { interpreter :: Maybe Text 
    , compiler    :: Maybe Text }
    deriving (Show)

------------------------------------
-- JSON/YAML parsing:
------------------------------------
instance FromJSON Aliases where
    parseJSON = withObject "Aliases" $ \obj -> Aliases
        <$> (obj .:? "interpreter")
        <*> (obj .:? "compiler"   )

instance ToJSON Aliases where
    toJSON alias = object
        [ "interpreter" .= interpreter alias
        , "compiler"    .= compiler alias    ]

instance FromJSON HenHenConfig where
    parseJSON = withObject "HenHenConfig" $ \obj -> HenHenConfig
        <$> optional mempty (obj .:? "dependencies")
        <*> optional mempty (obj .:? "special-dependencies")
        <*> (obj .:? "aliases")
        where
            optional :: (Functor f) => a -> f (Maybe a) -> f a
            optional = fmap . fromMaybe

instance ToJSON HenHenConfig where
    toJSON config = object
        [ "dependencies"         .= chickenDeps config
        , "special-dependencies" .= specialDeps config
        , "aliases"              .= aliases config     ]

------------------------------------
-- Utils:
------------------------------------
configFieldOrderMap :: HashMap Text Int
configFieldOrderMap = HashMap.fromList
    [ ("dependencies"        , 1)
    , ("special-dependencies", 2)
    , ("aliases"             , 3) ]

configFieldOrder :: Text -> Text -> Ordering
configFieldOrder = compare `on` (`HashMap.lookup` configFieldOrderMap)
