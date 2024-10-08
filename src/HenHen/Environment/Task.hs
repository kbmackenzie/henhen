{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}

module HenHen.Environment.Task
( EnvironmentTask(..)
, runEnvironmentTask
) where

import HenHen.Packager (Packager, throwError)
import HenHen.Environment.Type (Environment)
import System.Process.Typed (proc, setEnv, setWorkingDir, runProcess, ExitCode(..))
import Data.Maybe (fromMaybe)

data EnvironmentTask = EnvironmentTask
    { taskCommand     :: String
    , taskArguments   :: [String]
    , taskDirectory   :: Maybe FilePath
    , taskErrorReport :: Maybe (String -> String) }

runEnvironmentTask :: Environment -> EnvironmentTask -> Packager ()
runEnvironmentTask env task = do
    let setLocation = maybe id setWorkingDir (taskDirectory task)
    let setters = setEnv env . setLocation
    let process = setters $ proc (taskCommand task) (taskArguments task)

    runProcess process >>= \case
        ExitSuccess     -> return ()
        (ExitFailure n) -> do
            let report  = fromMaybe id (taskErrorReport task)
            let command = unwords (taskCommand task : taskArguments task)
            let message = concat ["process ", show command, " exited with code: ", show n]
            (throwError . report) message
