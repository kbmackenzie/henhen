module HenHen.Environment.Folders
( localChicken
, localBuild
, localChickenBin
, localDependencies
) where

import System.FilePath ((</>))

localChicken :: FilePath
localChicken = ".henhen"

localBuild :: FilePath
localBuild = localChicken </> "_build"

localChickenBin :: FilePath
localChickenBin = localChicken </> "bin"

localDependencies :: FilePath
localDependencies = localChicken </> "_dependencies"
