module HenHen.Actions.Run
( runBinary
) where

import HenHen.Environment
    ( EnvironmentTask(..)
    , localChickenBin
    )
import HenHen.Utils.FilePath (toExecutablePath)
import System.FilePath ((</>))

runBinary :: String -> EnvironmentTask
runBinary name = do
    let bin = localChickenBin </> toExecutablePath name
    let failMessage :: String -> String
        failMessage message = concat ["Failure when running target ", show name, ": ", message]

    EnvironmentTask
        { taskCommand     = bin
        , taskArguments   = []
        , taskDirectory   = Nothing
        , taskErrorReport = Just failMessage
        , afterTask       = Nothing }
