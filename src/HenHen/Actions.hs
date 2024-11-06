module HenHen.Actions
( Action(..)
, runAction
, collectDependencies
) where

import HenHen.Config (HenHenConfig(..), getConfig, hasConfig)
import HenHen.Logger (LogLevel(..))
import HenHen.Packager (Packager, throwError)
import HenHen.Environment (createEnvironment, runEnvironmentTask)
import HenHen.Actions.Build (buildAll)
import HenHen.Actions.Run (run)
import HenHen.Actions.Prepare (prepare, collectDependencies)
import HenHen.Actions.Clean (clean, purge)
import HenHen.Actions.Init (initialize)
import HenHen.Actions.Interpret (interpret)
import HenHen.Actions.Install (install)
import HenHen.Actions.Copy (copy)
import HenHen.Actions.Type (Action(..))
import HenHen.Actions.REPL (repl)

runAction :: Action -> Maybe LogLevel -> Packager ()
runAction (Init name) _ = do
    isHenHen <- hasConfig
    if isHenHen
        then throwError "There's already a project in the current directory!"
        else initialize name
runAction (Install name) verbosity = do
    install name
    runAction Build verbosity
runAction (Clean shouldPurge) _ = do
    isHenHen <- hasConfig
    if isHenHen
        then (if shouldPurge then purge else clean)
        else throwError "Cannot clean directory: No config file found, possible mistake?"
runAction action verbosity = do
    config <- setVerbosity verbosity <$> getConfig
    env    <- createEnvironment config
    prepare config env
    buildAll config env
    case action of
        Build              -> return ()
        (Run name args)    -> run config env name args
        (Interpret source) -> runEnvironmentTask config env (interpret config source)
        (Copy name dest)   -> copy config name dest
        REPL               -> repl config env

setVerbosity :: Maybe LogLevel -> HenHenConfig -> HenHenConfig
setVerbosity verbosity config = do
    let setter = maybe id const verbosity
    config { configLogLevel = setter (configLogLevel config) }
