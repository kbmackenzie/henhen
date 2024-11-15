{-# LANGUAGE LambdaCase #-}

module HenHen.CLI.Run
( run
) where

import HenHen (henhen)
import HenHen.Config (LogLevel(..))
import HenHen.CLI.Options (HenHenCommand(..), getCommand)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitWith, ExitCode(..))

run :: IO ()
run = do
    command <- getCommand
    let action    = commandAction command
    let verbosity = getVerbosity  command
    henhen action verbosity >>= \case
        (Left e)  -> do
            hPutStrLn stderr ("[henhen] " ++ e)
            exitWith (ExitFailure 1)
        (Right _) -> return ()

getVerbosity :: HenHenCommand -> Maybe LogLevel
getVerbosity command = case (commandQuiet command, commandVerbose command) of
    (True, _   ) -> Just Quiet
    (_   , True) -> Just Verbose
    _            -> Nothing
