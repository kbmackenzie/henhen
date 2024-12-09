module HenHen.Actions.Copy
( copy
) where

import HenHen.Config
    ( HenHenConfig(..)
    , Target(..)
    , TargetKey(..)
    )
import HenHen.Logger (logMessage)
import HenHen.Environment (localBuild)
import HenHen.Packager (Packager, throwError)
import HenHen.Utils.FilePath (toExecutablePath)
import HenHen.Utils.IO (copyFileSafe)
import System.FilePath ((</>))
import qualified Data.HashMap.Strict as HashMap

copy :: HenHenConfig -> String -> FilePath -> Packager ()
copy config name destination = do
    let failMessage :: String -> String
        failMessage message = concat ["Can't copy target ", show name, ": ", message]

    binary <- case HashMap.lookup (TargetKey name) (configTargets config) of
        (Just (Executable _ _)) -> return (localBuild </> toExecutablePath name)
        (Just (Egg        _ _)) -> throwError (failMessage "target isn't an executable")
        Nothing                 -> throwError (failMessage "no target matches that name")
    let output = destination </> toExecutablePath name

    logMessage (configLogLevel config) . concat $
        ["Copying ", show binary, " to ", show destination]
    copyFileSafe binary output
