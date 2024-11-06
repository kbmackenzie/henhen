module HenHen.Actions.REPL
( repl
) where

import HenHen.Config
    ( HenHenConfig(..)
    , LogLevel(..)
    , getInterpreter
    )
import HenHen.Environment
    ( Environment
    , EnvironmentTask(..)
    , runEnvironmentTask
    )
import HenHen.Packager (Packager)

repl :: HenHenConfig -> Environment -> Packager ()
repl config env = do
    let interpreter = getInterpreter config
    let failMessage :: String -> String
        failMessage message = "Failure when running REPL: " ++ message

    let replConfig = config { configLogLevel = Verbose }
    runEnvironmentTask replConfig env $ EnvironmentTask
        { taskCommand     = interpreter
        , taskArguments   = []
        , taskDirectory   = Nothing
        , taskErrorReport = Just failMessage
        , afterTask       = Nothing }
