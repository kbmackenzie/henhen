module HenHen.Actions.Interpret
( interpret
) where

import HenHen.Environment (EnvironmentTask(..))
import HenHen.Config (HenHenConfig(..), getInterpreter)

interpret :: HenHenConfig -> FilePath -> EnvironmentTask
interpret config source = do
    let interpreter = getInterpreter config
    let failMessage :: String -> String
        failMessage message = concat [ "Failure when running script ", show source, ": ", message ]

    EnvironmentTask
        { taskCommand     = interpreter
        , taskArguments   = ["-s", source, "-b"]
        , taskDirectory   = Nothing
        , taskErrorReport = Just failMessage
        , afterTask       = Nothing }
