module HenHen.Actions
( Action(..)
, runAction
, collectDependencies
) where

import HenHen.Config (readConfig, hasConfig)
import HenHen.Packager (Packager, throwError)
import HenHen.Environment (createEnvironment, runEnvironmentTask)
import HenHen.Actions.Build (buildAll)
import HenHen.Actions.Run (run)
import HenHen.Actions.Prepare (prepare, collectDependencies)
import HenHen.Actions.Clean (clean, purge)
import HenHen.Actions.Init (initialize)
import HenHen.Actions.Interpret (interpret)
import HenHen.Actions.Install (install)
import HenHen.Actions.Type (Action(..))

runAction :: Action -> Packager ()
runAction (Init name) = do
    isHenHen <- hasConfig
    if isHenHen
        then throwError "There's already a project in the current directory!"
        else initialize name
runAction (Install name) = do
    install name
    runAction Build
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
        (Run name args) -> do
            buildAll config env
            run config env name args
        (Interpret source) -> do
            buildAll config env
            runEnvironmentTask env (interpret config source)
