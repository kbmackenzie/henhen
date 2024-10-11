module HenHen.Cache.Manage
( cachePath
, readCache
, tryGetCache
, writeCache
) where

import HenHen.Packager (Packager, catchError, liftEither)
import HenHen.Cache.Type (CacheData(..))
import HenHen.Utils.Yaml (readYaml)
import HenHen.Utils.IO (readFileSafe, writeFileSafe)
import HenHen.Environment (localChicken)
import System.FilePath ((</>))
import Data.Yaml (encode)

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
