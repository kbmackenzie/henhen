{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}

module HenHen.Environment.Task
( EnvironmentTask(..)
, runEnvironmentTask
) where

import HenHen.Packager (Packager, throwError)
import HenHen.Config (HenHenConfig(..))
import HenHen.Logger (LogLevel(..))
import HenHen.Environment.Type (Environment)
import System.Process.Typed
    ( proc
    , setEnv
    , setWorkingDir
    , runProcess
    , ExitCode(..)
    , setStdout
    , nullStream
    )
import Data.Maybe (fromMaybe)

data EnvironmentTask = EnvironmentTask
    { taskCommand     :: String
    , taskArguments   :: [String]
    , taskDirectory   :: Maybe FilePath
    , taskErrorReport :: Maybe (String -> String)
    , afterTask       :: Maybe (Packager ()) }

runEnvironmentTask :: HenHenConfig -> Environment -> EnvironmentTask -> Packager ()
runEnvironmentTask config env task = do
    let setLocation = maybe id setWorkingDir (taskDirectory task)
    let setStreams  = case configLogLevel config of
            (Just Verbose) -> id
            _              -> setStdout nullStream
    let setters = setStreams . setEnv env . setLocation
    let process = setters $ proc (taskCommand task) (taskArguments task)

    runProcess process >>= \case
        ExitSuccess     -> return ()
        (ExitFailure n) -> do
            let report  = fromMaybe id (taskErrorReport task)
            let command = unwords (taskCommand task : taskArguments task)
            let message = concat ["process ", show command, " exited with code: ", show n]
            (throwError . report) message
    sequence_ (afterTask task)
