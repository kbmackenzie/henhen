module HenHen.Environment.Folders
( localChicken
, localBuild
, localChickenBin
) where

import System.FilePath ((</>))

localChicken :: FilePath
localChicken = ".chicken"

localBuild :: FilePath
localBuild = localChicken </> "henhen"

localChickenBin :: FilePath
localChickenBin = localChicken </> "bin"
