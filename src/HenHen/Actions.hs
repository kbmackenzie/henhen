module HenHen.Actions
( Action(..)
, runAction
) where

import HenHen.Config (HenHenConfig(..))
import HenHen.Packager (Packager)
import HenHen.Environment (createEnvironment, runEnvironmentTask)
import HenHen.Actions.Build (buildAll)
import HenHen.Actions.Clean (clean, purge)
import HenHen.Actions.Interpret (interpret)
import HenHen.Actions.Run (run)
import HenHen.Actions.Prepare (prepare)
import HenHen.Actions.Type (Action(..))

runAction :: HenHenConfig -> Action -> Packager ()
runAction _      (Clean p) = if p then purge else clean
runAction config action = do
    prepare config
    env <- createEnvironment config
    case action of
        Build -> do
            buildAll config env
        (Run name) -> do
            buildAll config env
            run config env name
        (Interpret source) -> do
            buildAll config env
            runEnvironmentTask env (interpret config source)
