module HenHen.Actions.Prepare
( prepare
) where

import HenHen.Config
    ( HenHenConfig(..)
    , TargetKey(..)
    , TargetMeta(..)
    , getTargetMeta
    , configPath
    )
import HenHen.Packager (Packager)
import HenHen.Environment
    ( Environment
    , fetch
    , runEnvironmentTask
    , localChicken
    , localBuild
    , localChickenBin
    )
import HenHen.Cache (CacheInfo(..), tryGetCache, writeCache)
import HenHen.Utils.IO (createDirectory, globFiles, copyFileSafe, getFileModTime)
import HenHen.Utils.Time (getPosixTimeInSeconds)
import Data.Maybe (fromMaybe)
import System.FilePath ((</>))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Control.Monad (when, unless)

gatherDependencies :: HenHenConfig -> HashSet String
gatherDependencies config = do
    let topLevel = configDeps config
    let isTarget :: TargetKey -> Bool
        isTarget = flip HashMap.member (configTargets config)

    let targets    = HashMap.elems (configTargets config)
    let targetDeps = concatMap (filter isTarget . metaDeps . getTargetMeta) targets
    HashSet.union topLevel (HashSet.fromList $ map getKey targetDeps)

installDependencies :: HenHenConfig -> Environment -> CacheInfo -> Packager ()
installDependencies config env cache = do
    let install :: String -> Packager ()
        install dep = do
            let hasCached = HashSet.member dep (dependencySet cache)
            unless hasCached $ do
                let tasks = fetch config dep
                mapM_ (runEnvironmentTask env) tasks
    let dependencies = HashSet.toList (gatherDependencies config)
    mapM_ install dependencies

emptyCache :: CacheInfo
emptyCache = CacheInfo
    { buildTime     = 0
    , dependencySet = mempty }

updateCache :: HenHenConfig -> Packager ()
updateCache config = do
    timestamp <- getPosixTimeInSeconds
    let dependencies = gatherDependencies config

    writeCache $ CacheInfo
        { buildTime     = timestamp
        , dependencySet = dependencies }

prepare :: HenHenConfig -> Environment -> Packager ()
prepare config env = do
    mapM_ (createDirectory False)
        [ localChicken
        , localBuild
        , localChickenBin ]

    cache      <- fromMaybe emptyCache <$> tryGetCache
    configTime <- getFileModTime configPath
    let outdated = buildTime cache < configTime

    when outdated $ do
        installDependencies config env cache
        updateCache config

    let sourceDir = fromMaybe "." (configSourceDir config)
    let patterns  = configSources config ++ configDataFiles config
    files <- globFiles sourceDir patterns

    let copy :: FilePath -> Packager ()
        copy path = copyFileSafe path (localBuild </> path)
    mapM_ copy files
