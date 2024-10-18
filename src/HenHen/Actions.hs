module HenHen.Actions
( Action(..)
, runAction
) where

import HenHen.Config (readConfig, hasConfig)
import HenHen.Packager (Packager, throwError)
import HenHen.Environment (createEnvironment, runEnvironmentTask)
import HenHen.Actions.Build (buildAll)
import HenHen.Actions.Clean (clean, purge)
import HenHen.Actions.Interpret (interpret)
import HenHen.Actions.Run (run)
import HenHen.Actions.Prepare (prepare)
import HenHen.Actions.Type (Action(..))

runAction :: Action -> Packager ()
runAction (Clean shouldPurge) = do
    isHenHen <- hasConfig
    if isHenHen
        then (if shouldPurge then purge else clean)
        else throwError "Cannot clean directory: No config file found, possible mistake?"
runAction action = do
    config <- readConfig
    env    <- createEnvironment config
    prepare config env
    case action of
        Build -> do
            buildAll config env
        (Run name) -> do
            buildAll config env
            run config env name
        (Interpret source) -> do
            buildAll config env
            runEnvironmentTask env (interpret config source)
