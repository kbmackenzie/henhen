module HenHen.Actions.Prepare
( collectDependencies
, prepare
) where

import HenHen.Config
    ( HenHenConfig(..)
    , TargetKey(..)
    , TargetMeta(..)
    , getTargetMeta
    , projectConfig
    )
import HenHen.Packager (Packager)
import HenHen.Environment
    ( Environment
    , fetch
    , runEnvironmentTask
    , localChicken
    , localBuild
    , localChickenBin
    , localDependencies
    )
import HenHen.Logger (logMessage)
import HenHen.Cache (CacheInfo(..), tryGetCache, writeCache)
import HenHen.Utils.IO
    ( createDirectory
    , globFiles
    , copyFileSafe
    , getFileModTime
    , exists
    , EntryType(..)
    )
import HenHen.Utils.Time (getPosixTimeInSeconds)
import Data.Maybe (fromMaybe)
import System.FilePath ((</>), takeDirectory)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Data.Containers.ListUtils (nubOrd)
import Control.Monad (when, unless)

collectDependencies :: HenHenConfig -> HashSet String
collectDependencies config = do
    let topLevel = configDeps config
    let isNotTarget :: TargetKey -> Bool
        isNotTarget = not . flip HashMap.member (configTargets config)

    let targets    = HashMap.elems (configTargets config)
    let targetDeps = concatMap (filter isNotTarget . metaDeps . getTargetMeta) targets
    HashSet.union topLevel (HashSet.fromList $ map getKey targetDeps)

installDependencies :: HenHenConfig -> Environment -> CacheInfo -> Packager ()
installDependencies config env cache = do
    let install :: String -> Packager ()
        install dep = do
            let hasCached = HashSet.member dep (dependencySet cache)
            unless hasCached $ do
                logMessage (configLogLevel config) ("Installing dependency " ++ show dep)
                let tasks = fetch config dep
                mapM_ (runEnvironmentTask config env) tasks
    let dependencies = HashSet.toList (collectDependencies config)
    mapM_ install dependencies

emptyCache :: CacheInfo
emptyCache = CacheInfo
    { buildTime     = 0
    , dependencySet = mempty }

updateCache :: HenHenConfig -> Packager ()
updateCache config = do
    timestamp <- getPosixTimeInSeconds
    let dependencies = collectDependencies config

    writeCache $ CacheInfo
        { buildTime     = timestamp
        , dependencySet = dependencies }

prepare :: HenHenConfig -> Environment -> Packager ()
prepare config env = do
    mapM_ (createDirectory False)
        [ localChicken
        , localBuild
        , localChickenBin
        , localDependencies ]

    logMessage (configLogLevel config) "Preparing..."
    cache      <- fromMaybe emptyCache <$> tryGetCache
    configTime <- getFileModTime projectConfig
    let outdated = buildTime cache < configTime

    when outdated $ do
        logMessage (configLogLevel config) "Installing dependencies..."
        installDependencies config env cache
        updateCache config

    let sourceDir = fromMaybe "." (configSourceDir config)
    sourceFiles <- globFiles sourceDir (configSources config)
    dataFiles   <- globFiles "." (configDataFiles config)

    let files = nubOrd (sourceFiles ++ dataFiles)

    let copy :: FilePath -> Packager ()
        copy path = do
            let destination = localBuild </> path
            createDirectory True (takeDirectory destination)

            alreadyExists <- exists File destination
            if alreadyExists
                then do
                    sourceMod <- getFileModTime path
                    destMod   <- getFileModTime destination
                    when (sourceMod > destMod) $
                        copyFileSafe path destination
                else copyFileSafe path destination
    mapM_ copy files
