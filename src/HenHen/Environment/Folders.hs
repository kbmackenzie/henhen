module HenHen.Environment.Folders
( localChicken
, localBuild
) where

import System.FilePath ((</>))

localChicken :: FilePath
localChicken = ".chicken"

localBuild :: FilePath
localBuild = localChicken </> "henhen"
