{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module HenHen.Config.Type
( HenHenConfig(..)
, Aliases(..)
, configFieldOrder
, getInstaller
, getCompiler
, getInterpreter
, getStatus
, getUninstaller
, getSourcePath
) where

import HenHen.Config.Target (Target, MetaKey, getTargetMap)
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
import HenHen.Utils.Maybe (optional)
import qualified Data.HashMap.Strict as HashMap
import Data.Function (on)
import Data.Maybe (fromMaybe)
import System.FilePattern (FilePattern)
import System.FilePath ((</>), normalise)

data HenHenConfig = HenHenConfig
    { configName      :: Maybe String             -- Project name.
    , configSources   :: [FilePattern]            -- Patterns for matching source files.
    , configDataFiles :: [FilePattern]            -- Patterns for matching data files.
    , configSourceDir :: Maybe FilePath           -- Source root.
    , configDeps      :: HashSet String           -- Project dependencies.
    , configFetch     :: HashMap String String    -- From where to fetch custom dependencies.
    , configAliases   :: Maybe Aliases            -- Aliases for CHICKEN Scheme binaries.
    , configTargets   :: HashMap MetaKey Target } -- Build targets.

data Aliases = Aliases
    { installerAlias   :: Maybe String
    , compilerAlias    :: Maybe String
    , interpreterAlias :: Maybe String
    , statusAlias      :: Maybe String
    , uninstallerAlias :: Maybe String }

------------------------------------
-- JSON/YAML parsing:
------------------------------------
instance FromJSON Aliases where
    parseJSON = withObject "Aliases" $ \obj -> Aliases
        <$> (obj .:? "installer"  )
        <*> (obj .:? "compiler"   )
        <*> (obj .:? "interpreter")
        <*> (obj .:? "status"     )
        <*> (obj .:? "uninstaller")

instance ToJSON Aliases where
    toJSON aliases = object
        [ "installer"   .= installerAlias aliases
        , "compiler"    .= compilerAlias aliases
        , "interpreter" .= interpreterAlias aliases
        , "status"      .= statusAlias aliases
        , "uninstaller" .= uninstallerAlias aliases ]

instance FromJSON HenHenConfig where
    parseJSON = withObject "HenHenConfig" $ \obj -> HenHenConfig
        <$> (obj .:? "name")
        <*> optional mempty (obj .:? "source-files")
        <*> optional mempty (obj .:? "data-files")
        <*> (obj .:? "source-folder")
        <*> optional mempty (obj .:? "dependencies")
        <*> optional mempty (obj .:? "fetch")
        <*> (obj .:? "aliases")
        <*> fmap getTargetMap (optional mempty (obj .:? "targets"))

instance ToJSON HenHenConfig where
    toJSON config = object
        [ "name"          .= configName config
        , "source-files"  .= configSources config
        , "data-files"    .= configDataFiles config
        , "source-folder" .= configSourceDir config
        , "dependencies"  .= configDeps config
        , "fetch"         .= configFetch config
        , "aliases"       .= configAliases config 
        , "targets"       .= HashMap.elems (configTargets config) ]

------------------------------------
-- Pretty-printing:
------------------------------------
configFieldOrderMap :: HashMap Text Int
configFieldOrderMap = HashMap.fromList
    [ ("name"          , 1)
    , ("source-files"  , 2)
    , ("data-files"    , 3)
    , ("source-folder" , 4)
    , ("dependencies"  , 5)
    , ("fetch"         , 6)
    , ("aliases"       , 7)
    , ("targets"       , 8) ]

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

getStatus :: HenHenConfig -> String
getStatus = getAlias "chicken-status" statusAlias

getUninstaller :: HenHenConfig -> String
getUninstaller = getAlias "chicken-uninstall" uninstallerAlias

getSourcePath :: HenHenConfig -> FilePath -> FilePath
getSourcePath = maybe id ((</>) . normalise) . configSourceDir
