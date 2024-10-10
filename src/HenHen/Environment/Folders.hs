module HenHen.Environment.Folders
( localChicken
, chickenBuild
) where

import System.FilePath ((</>))

localChicken :: FilePath
localChicken = ".chicken"

chickenBuild :: FilePath
chickenBuild = localChicken </> "build"
