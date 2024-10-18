module HenHen.Actions.Run
( run
) where

import HenHen.Packager (Packager, throwError)
import HenHen.Config (HenHenConfig(..))
import HenHen.Environment
    ( Environment
    , localChickenBin
    , EnvironmentTask(..)
    , runEnvironmentTask
    )
import HenHen.Utils.FilePath (toExecutablePath)
import System.FilePath ((</>))
import System.Process.Typed (shell, setEnv, ExitCode(..), runProcess)
import qualified Data.HashMap.Strict as HashMap

runBinary :: String -> EnvironmentTask
runBinary name = do
    let bin = localChickenBin </> toExecutablePath name
    let failMessage :: String -> String
        failMessage message = concat ["Failure when running target ", show name, ": ", message]

    EnvironmentTask
        { taskCommand     = bin
        , taskArguments   = []
        , taskDirectory   = Nothing
        , taskErrorReport = Just failMessage
        , afterTask       = Nothing }

runScript :: Environment -> String -> String -> Packager ()
runScript env name line = do
    let process = setEnv env $ shell line
    exitCode <- runProcess process
    case exitCode of
        ExitSuccess     -> return ()
        (ExitFailure n) -> throwError . concat $ ["Script ", show name, " exited with code ", show n, "!"]

run :: HenHenConfig -> Environment -> String -> Packager ()
run config env name = do
    let script = HashMap.lookup name (configScripts config)
    case script of
        (Just line) -> runScript env name line
        Nothing     -> runEnvironmentTask env (runBinary name)
