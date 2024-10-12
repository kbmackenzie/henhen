module HenHen.Actions.Prepare
( prepare
) where

import HenHen.Config (HenHenConfig(..))
import HenHen.Packager (Packager)
import HenHen.Environment
    ( localChicken
    , localBuild
    , localChickenBin
    )
import HenHen.Utils.IO (createDirectory, globFiles, copyFileSafe)
import Data.Maybe (fromMaybe)
import System.FilePath ((</>))

prepare :: HenHenConfig -> Packager ()
prepare config = do
    mapM_ (createDirectory False)
        [ localChicken
        , localBuild
        , localChickenBin ]

    let sourceDir = fromMaybe "." (configSourceDir config)
    let patterns  = configSources config ++ configDataFiles config
    files <- globFiles sourceDir patterns

    let copy :: FilePath -> Packager ()
        copy path = copyFileSafe path (localBuild </> path)
    mapM_ copy files
