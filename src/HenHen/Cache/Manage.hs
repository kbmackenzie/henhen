{-# LANGUAGE TupleSections #-}

module HenHen.Cache.Manage
( cachePath
, readCache
, tryGetCache
, writeCache
, createCache
) where

import HenHen.Packager (Packager, catchError, liftEither)
import HenHen.Cache.Type (CacheData(..))
import HenHen.Utils.Yaml (readYaml)
import HenHen.Utils.IO (readFileSafe, getFileModTime, writeFileSafe)
import HenHen.Utils.Time (getPosixTimeInSeconds)
import HenHen.Environment (localChicken)
import System.FilePath ((</>))
import Data.Yaml (encode)
import qualified Data.HashMap.Strict as HashMap

cachePath :: FilePath
cachePath = localChicken </> "build-info.json"

readCache :: Packager CacheData
readCache = do
    contents <- readFileSafe cachePath
    liftEither $ readYaml contents

tryGetCache :: Packager (Maybe CacheData)
tryGetCache = fmap Just readCache `catchError` \_ -> return Nothing

writeCache :: CacheData -> Packager ()
writeCache = writeFileSafe cachePath . encode

createCache :: [FilePath] -> Packager CacheData
createCache files = do
    let checkFile :: FilePath -> Packager (FilePath, Integer)
        checkFile file = (file,) <$> getFileModTime file

    buildTime_ <- getPosixTimeInSeconds
    fileMap_   <- HashMap.fromList <$> mapM checkFile files
    return CacheData
        { buildTime = buildTime_
        , fileMap   = fileMap_   }
