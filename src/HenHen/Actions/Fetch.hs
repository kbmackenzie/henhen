module HenHen.Actions.Fetch
( URL
, fetch
) where

import HenHen.Environment (localDependencies, EnvironmentTask(..))
import HenHen.Config (HenHenConfig(..))
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

installEgg :: FilePath -> EnvironmentTask
installEgg path = do
    let failMessage :: String -> String
        failMessage message = concat [ "Couldn't install egg ", show path, ": ", message ]

    EnvironmentTask
        { taskCommand     = "chicken-install"
        , taskArguments   = []
        , taskDirectory   = Nothing
        , taskErrorReport = Just failMessage
        , afterTask       = Nothing }

fetchEgg :: String -> URL -> [EnvironmentTask]
fetchEgg name url = do
    let directory = localDependencies </> name
    [fetchRepository name url, installEgg directory]

normalEgg :: String -> EnvironmentTask
normalEgg name = do
    let failMessage :: String -> String
        failMessage message = concat ["Couldn't install egg ", show name, ": ", message]

    EnvironmentTask
        { taskCommand     = "chicken-install"
        , taskArguments   = [name]
        , taskDirectory   = Nothing
        , taskErrorReport = Just failMessage
        , afterTask       = Nothing }

fetch :: HenHenConfig -> String -> [EnvironmentTask]
fetch config name = do
    let maybeUrl = HashMap.lookup name (configFetch config)
    case maybeUrl of
        (Just url) -> fetchEgg name url
        Nothing    -> return (normalEgg name)
