{-# LANGUAGE TupleSections #-}

module HenHen.Cache.Manage
( cachePath
, readCache
, tryGetCache
, writeCache
, createCache
) where

import HenHen.Packager (Packager, catchError, liftEither)
import HenHen.Cache.Type (CacheInfo(..))
import HenHen.Utils.IO (readFileSafe, getFileModTime, writeFileSafe)
import HenHen.Utils.Json (readJson, writeJson)
import HenHen.Utils.Time (getPosixTimeInSeconds)
import HenHen.Environment (localChicken)
import System.FilePath ((</>))
import qualified Data.HashMap.Strict as HashMap

cachePath :: FilePath
cachePath = localChicken </> "build-info.json"

readCache :: Packager CacheInfo
readCache = do
    contents <- readFileSafe cachePath
    liftEither $ readJson contents

tryGetCache :: Packager (Maybe CacheInfo)
tryGetCache = fmap Just readCache `catchError` \_ -> return Nothing

writeCache :: CacheInfo -> Packager ()
writeCache = writeFileSafe cachePath . writeJson

createCache :: [FilePath] -> Packager CacheInfo
createCache files = do
    let checkFile :: FilePath -> Packager (FilePath, Integer)
        checkFile file = (file,) <$> getFileModTime file

    buildTime_ <- getPosixTimeInSeconds
    targetMap_ <- HashMap.fromList <$> mapM checkFile files
    return CacheInfo
        { buildTime = buildTime_
        , targetMap = targetMap_ }
