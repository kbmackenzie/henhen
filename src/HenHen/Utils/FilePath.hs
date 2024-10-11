module HenHen.Utils.FilePath
( toExecutablePath
) where

import System.Info (os)
import System.FilePath (dropExtension, replaceExtension)

toExecutablePath :: FilePath -> FilePath
toExecutablePath = case os of
    "mingw32" -> (`replaceExtension` "exe")
    _         -> dropExtension
