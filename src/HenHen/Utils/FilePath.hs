module HenHen.Utils.FilePath
( toExecutablePath
) where

import System.FilePath (replaceExtension)
import System.Directory (exeExtension)

toExecutablePath :: FilePath -> FilePath
toExecutablePath = flip replaceExtension exeExtension
