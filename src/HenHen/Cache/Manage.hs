{-# LANGUAGE TupleSections #-}

module HenHen.Cache.Manage
( cachePath
, readCache
, tryGetCache
, writeCache
) where

import HenHen.Packager (Packager, catchError, liftEither)
import HenHen.Cache.Type (CacheInfo(..))
import HenHen.Utils.IO (readFileSafe, writeFileSafe)
import HenHen.Utils.Json (readJson, writeJson)
import HenHen.Environment (localChicken)
import System.FilePath ((</>))

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
