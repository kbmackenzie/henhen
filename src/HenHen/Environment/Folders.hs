module HenHen.Environment.Folders
( localChicken
, localBuild
, localChickenBin
, localChickenRepo
, localDependencies
) where

import System.FilePath ((</>))

localChicken :: FilePath
localChicken = ".henhen"

localBuild :: FilePath
localBuild = localChicken </> "_build"

localChickenBin :: FilePath
localChickenBin = localChicken </> "bin"

localChickenRepo :: FilePath
localChickenRepo = localChicken </> "lib"

localDependencies :: FilePath
localDependencies = localChicken </> "_fetch"
