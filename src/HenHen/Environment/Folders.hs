module HenHen.Environment.Folders
( localChicken
, localBuild
, localChickenBin
, localDependencies
) where

import System.FilePath ((</>))

localChicken :: FilePath
localChicken = ".chicken"

localBuild :: FilePath
localBuild = localChicken </> "_build"

localChickenBin :: FilePath
localChickenBin = localChicken </> "bin"

localDependencies :: FilePath
localDependencies = localChicken </> "_dependencies"
