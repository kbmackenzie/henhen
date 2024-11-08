module HenHen.Config.Manage
( projectConfig
, globalAliases
, readProjectConfig
, readGlobalAliases
, getConfig
, hasProjectConfig
, writeProjectConfig
) where

import HenHen.Config.Type
    ( HenHenConfig(..)
    , Aliases(..)
    , configFieldOrder
    , aliasUnion
    )
import HenHen.Packager (Packager, liftIO, liftEither, throwError, catchError)
import HenHen.Utils.IO (readFileSafe, writeFileSafe, exists, EntryType(..), whenFileExists)
import HenHen.Utils.Yaml (readYaml, prettyYaml)
import Data.Aeson (ToJSON)
import System.FilePath ((</>))
import System.Directory (getHomeDirectory)
import Control.Monad ((>=>))

projectConfig :: FilePath
projectConfig = "henhen.yaml"

globalAliases :: IO FilePath
globalAliases = (</> ".henhen-alias.yaml") <$> getHomeDirectory

readProjectConfig :: Packager HenHenConfig
readProjectConfig = do
    let readConfig_ :: FilePath -> Packager HenHenConfig
        readConfig_ = readFileSafe >=> liftEither . readYaml

    readConfig_ projectConfig `catchError` \message -> do
        let newMessage = "Couldn't read project config: " ++ message
        throwError newMessage

readGlobalAliases :: Packager (Maybe Aliases)
readGlobalAliases = do
    let readAliases :: FilePath -> Packager Aliases
        readAliases = readFileSafe >=> liftEither . readYaml

    path <- liftIO globalAliases
    whenFileExists readAliases path `catchError` \message -> do
        let newMessage = concat ["Couldn't read alias file ", show path, ": ", message]
        throwError newMessage

getConfig :: Packager HenHenConfig
getConfig = do
    config  <- readProjectConfig
    aliases <- readGlobalAliases
    let combineAliases = Just . maybe id aliasUnion aliases
    let getAllAliases  = maybe aliases combineAliases . configAliases
    return config
        { configAliases = getAllAliases config }

writeProjectConfig :: HenHenConfig -> Packager ()
writeProjectConfig = writeAsProjectConfig

writeAsProjectConfig :: (ToJSON a) => a -> Packager ()
writeAsProjectConfig value = do
    let yaml = prettyYaml configFieldOrder value
    writeFileSafe projectConfig yaml

hasProjectConfig :: Packager Bool
hasProjectConfig = exists File projectConfig
