{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}

module HenHen.Environment.Task
( EnvironmentTask(..)
, runEnvironmentTask
) where

import HenHen.Packager (Packager, throwError, liftIO)
import HenHen.Environment.Type (Environment)
import System.Process.Typed (proc, setEnv, setWorkingDir, runProcess, ExitCode(..))
import Data.Maybe (fromMaybe)

data EnvironmentTask = EnvironmentTask
    { taskCommand     :: String
    , taskArguments   :: [String]
    , taskDirectory   :: Maybe FilePath
    , taskErrorReport :: Maybe (String -> String)
    , taskPrepare     :: Maybe (IO ())
    , nextTask        :: Maybe EnvironmentTask   }

runEnvironmentTask :: Environment -> EnvironmentTask -> Packager ()
runEnvironmentTask env task = do
    let setLocation = maybe id setWorkingDir (taskDirectory task)
    let setters = setEnv env . setLocation
    let process = setters $ proc (taskCommand task) (taskArguments task)

    -- Run preparations:
    mapM_ liftIO (taskPrepare task)
    runProcess process >>= \case
        ExitSuccess     -> return ()
        (ExitFailure n) -> do
            let report  = fromMaybe id (taskErrorReport task)
            let command = unwords (taskCommand task : taskArguments task)
            let message = concat ["process ", show command, " exited with code: ", show n]
            (throwError . report) message
    mapM_ (runEnvironmentTask env) (nextTask task)
