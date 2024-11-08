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
, aliasUnion
) where

import HenHen.Config.Target (Target, TargetKey)
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
import HenHen.Logger (LogLevel(..))
import qualified Data.HashMap.Strict as HashMap
import Data.Function (on)
import Data.Maybe (fromMaybe)
import System.FilePattern (FilePattern)
import System.FilePath ((</>), normalise)
import Control.Applicative ((<|>))

data HenHenConfig = HenHenConfig
    { configName      :: Maybe String               -- Project name.
    , configSources   :: [FilePattern]              -- Patterns for matching source files.
    , configDataFiles :: [FilePattern]              -- Patterns for matching data files.
    , configSourceDir :: Maybe FilePath             -- Source root.
    , configDeps      :: HashSet String             -- Project dependencies.
    , configFetch     :: HashMap String String      -- From where to fetch custom dependencies.
    , configScripts   :: HashMap String String      -- Config scripts.
    , configAliases   :: Maybe Aliases              -- Aliases for CHICKEN Scheme binaries.
    , configTargets   :: HashMap TargetKey Target   -- Build targets.
    , configLogLevel  :: Maybe LogLevel             -- Log level; self-explanatory.
    }

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
        <*> (obj .:? "source-root")
        <*> optional mempty (obj .:? "dependencies")
        <*> optional mempty (obj .:? "fetch")
        <*> optional mempty (obj .:? "scripts")
        <*> (obj .:? "aliases")
        <*> optional mempty (obj .:? "targets")
        <*> (obj .:? "log-level")

instance ToJSON HenHenConfig where
    toJSON config = object
        [ "name"          .= configName config
        , "source-files"  .= configSources config
        , "data-files"    .= configDataFiles config
        , "source-root"   .= configSourceDir config
        , "dependencies"  .= configDeps config
        , "fetch"         .= configFetch config
        , "scripts"       .= configScripts config
        , "aliases"       .= configAliases config 
        , "targets"       .= configTargets config
        , "log-level"     .= configLogLevel config ]

------------------------------------
-- Pretty-printing:
------------------------------------
configFieldOrderMap :: HashMap Text Int
configFieldOrderMap = HashMap.fromList
    [ ("name"          , 1 )
    , ("source-files"  , 2 )
    , ("data-files"    , 3 )
    , ("source-root"   , 4 )
    , ("dependencies"  , 5 )
    , ("fetch"         , 6 )
    , ("scripts"       , 7 )
    , ("aliases"       , 8 )
    , ("targets"       , 9 )
    , ("log-level"     , 10) ]

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

aliasUnion :: Aliases -> Aliases -> Aliases
aliasUnion a b = Aliases
    { installerAlias   = installerAlias a   <|> installerAlias b
    , compilerAlias    = compilerAlias a    <|> compilerAlias b
    , interpreterAlias = interpreterAlias a <|> interpreterAlias b
    , statusAlias      = statusAlias a      <|> statusAlias b
    , uninstallerAlias = uninstallerAlias a <|> uninstallerAlias b }
