module HenHen.Actions.Interpret
( interpret
) where

import HenHen.Config
    ( HenHenConfig(..)
    , LogLevel(..)
    , getInterpreter
    )
import HenHen.Packager (Packager)
import HenHen.Environment
    ( Environment
    , EnvironmentTask(..)
    , runEnvironmentTask
    )

interpret :: HenHenConfig -> Environment -> FilePath -> Packager ()
interpret config env source = do
    let interpreter = getInterpreter config
    let failMessage :: String -> String
        failMessage message = concat [ "Failure when running script ", show source, ": ", message ]

    let runConfig = config { configLogLevel = Just Verbose }
    runEnvironmentTask runConfig env EnvironmentTask
        { taskCommand     = interpreter
        , taskArguments   = ["-s", source, "-b"]
        , taskDirectory   = Nothing
        , taskErrorReport = Just failMessage
        , afterTask       = Nothing }
