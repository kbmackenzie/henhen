module HenHen.Actions.Clean
( clean
, purge
) where

import HenHen.Packager (Packager)
import HenHen.Environment
    ( localChicken
    , localBuild
    )
import HenHen.Utils.IO (removeDirectory)

clean :: Packager ()
clean = removeDirectory localBuild

purge :: Packager ()
purge = removeDirectory localChicken
