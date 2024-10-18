module HenHen.Cache
( CacheInfo(..)
, readCache
, tryGetCache
, writeCache
) where

import HenHen.Cache.Type (CacheInfo(..))
import HenHen.Cache.Manage (readCache, tryGetCache, writeCache)
