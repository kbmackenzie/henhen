module HenHen.Actions.Clean
( clean
) where

import HenHen.Environment (localChicken)
import HenHen.Packager (Packager, liftIO)
import System.FilePattern.Directory (FilePattern, getDirectoryFiles)
import System.Directory (removeFile, removeDirectoryRecursive, doesDirectoryExist)
import Control.Monad (when)
import Data.Containers.ListUtils (nubOrd)

cleanPatterns :: [FilePattern]
cleanPatterns =
    [ "*.o" 
    , "*.so"
    , "*.import.scm"
    , "*.link" ]

softClean :: [FilePattern] -> Packager ()
softClean patterns = liftIO $
    getDirectoryFiles "." patterns >>= mapM_ removeFile . nubOrd

hardClean :: [FilePattern] -> Packager ()
hardClean patterns = do
    softClean patterns
    liftIO $ do
        hasChicken <- doesDirectoryExist localChicken
        when hasChicken (removeDirectoryRecursive localChicken)

clean :: [FilePattern] -> Bool -> Packager ()
clean patterns hard = do
    let cleaner = if hard then hardClean else softClean
    cleaner . nubOrd $ (patterns ++ cleanPatterns)
