module HenHen.Environment.Fetch
( URL
, fetch
) where

import HenHen.Environment.Folders (localDependencies)
import HenHen.Environment.Task (EnvironmentTask(..))
import HenHen.Config (HenHenConfig(..), getInstaller)
import System.FilePath ((</>))
import qualified Data.HashMap.Strict as HashMap

type URL = String

fetchRepository :: String -> URL -> EnvironmentTask
fetchRepository name url = do
    let directory = localDependencies </> name
    let failMessage :: String -> String
        failMessage message = concat [ "Couldn't fetch dependency ", show name, ": ", message ] 

    EnvironmentTask
        { taskCommand     = "git"
        , taskArguments   = ["clone", url, directory]
        , taskDirectory   = Nothing
        , taskErrorReport = Just failMessage
        , afterTask       = Nothing }

installEgg :: HenHenConfig -> String -> EnvironmentTask
installEgg config name = do
    let installer = getInstaller config
    let directory = localDependencies </> name

    let failMessage :: String -> String
        failMessage message = concat [ "Couldn't install egg ", show name, ": ", message ]

    EnvironmentTask
        { taskCommand     = installer
        , taskArguments   = []
        , taskDirectory   = Just directory
        , taskErrorReport = Just failMessage
        , afterTask       = Nothing }

fetchEgg :: HenHenConfig -> String -> URL -> [EnvironmentTask]
fetchEgg config name url = [fetchRepository name url, installEgg config name]

normalEgg :: HenHenConfig -> String -> EnvironmentTask
normalEgg config name = do
    let installer = getInstaller config
    let failMessage :: String -> String
        failMessage message = concat ["Couldn't install egg ", show name, ": ", message]

    EnvironmentTask
        { taskCommand     = installer
        , taskArguments   = [name]
        , taskDirectory   = Nothing
        , taskErrorReport = Just failMessage
        , afterTask       = Nothing }

fetch :: HenHenConfig -> String -> [EnvironmentTask]
fetch config name = do
    let maybeUrl = HashMap.lookup name (configFetch config)
    case maybeUrl of
        (Just url) -> fetchEgg config name url
        Nothing    -> return (normalEgg config name)
