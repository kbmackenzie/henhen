{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module HenHen.Config.Type
( HenHenConfig(..)
, Aliases(..)
, configFieldOrder
, getInstaller
, getCompiler
, getInterpreter
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
import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Function (on)
import Data.Maybe (fromMaybe)
import HenHen.Utils.Maybe (optional)

data HenHenConfig = HenHenConfig
    { configName    :: Maybe String
    , configDeps    :: HashSet String
    , specialDeps   :: HashMap String String
    , configAliases :: Maybe Aliases         }
    deriving (Show)

data Aliases = Aliases
    { installerAlias   :: Maybe String
    , compilerAlias    :: Maybe String
    , interpreterAlias :: Maybe String }
    deriving (Show)

------------------------------------
-- JSON/YAML parsing:
------------------------------------
instance FromJSON Aliases where
    parseJSON = withObject "Aliases" $ \obj -> Aliases
        <$> (obj .:? "installer"  )
        <*> (obj .:? "compiler"   )
        <*> (obj .:? "interpreter")

instance ToJSON Aliases where
    toJSON aliases = object
        [ "installer"   .= installerAlias aliases
        , "compiler"    .= compilerAlias aliases
        , "interpreter" .= interpreterAlias aliases ]

instance FromJSON HenHenConfig where
    parseJSON = withObject "HenHenConfig" $ \obj -> HenHenConfig
        <$> (obj .:? "name")
        <*> optional mempty (obj .:? "dependencies")
        <*> optional mempty (obj .:? "special-dependencies")
        <*> (obj .:? "aliases")

instance ToJSON HenHenConfig where
    toJSON config = object
        [ "dependencies"         .= configDeps config
        , "special-dependencies" .= specialDeps config
        , "aliases"              .= configAliases config     ]

------------------------------------
-- Pretty-printing:
------------------------------------
configFieldOrderMap :: HashMap Text Int
configFieldOrderMap = HashMap.fromList
    [ ("dependencies"        , 1)
    , ("special-dependencies", 2)
    , ("aliases"             , 3) ]

configFieldOrder :: Text -> Text -> Ordering
configFieldOrder = compare `on` (`HashMap.lookup` configFieldOrderMap)

------------------------------------
-- Utilities:
------------------------------------
getAlias :: a -> (Aliases -> Maybe a) -> HenHenConfig -> a
getAlias def getter config = fromMaybe def $ do
    aliases <- configAliases config
    getter aliases

getInstaller :: HenHenConfig -> String
getInstaller = getAlias "chicken-install" installerAlias

getCompiler :: HenHenConfig -> String
getCompiler = getAlias "csc" compilerAlias

getInterpreter :: HenHenConfig -> String
getInterpreter = getAlias "csi" interpreterAlias
