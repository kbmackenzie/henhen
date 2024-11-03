module HenHen.Actions.Clean
( clean
, purge
) where

import HenHen.Packager (Packager)
import HenHen.Environment
    ( localChicken
    , localBuild
    )
import HenHen.Utils.IO (removeDirectoryIfExists)

clean :: Packager ()
clean = removeDirectoryIfExists localBuild

purge :: Packager ()
purge = removeDirectoryIfExists localChicken
