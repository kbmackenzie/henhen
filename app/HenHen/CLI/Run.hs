{-# LANGUAGE LambdaCase #-}

module HenHen.CLI.Run
( run
) where

import HenHen (henhen)
import HenHen.CLI.Options (HenHenOptions(..), getOptions)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitWith, ExitCode(..))

run :: IO ()
run = do
    command <- getOptions
    let action   = getAction command
    let logLevel = getLogLevel command
    henhen action logLevel >>= \case
        (Left e)  -> do
            hPutStrLn stderr ("[henhen] " ++ e)
            exitWith (ExitFailure 1)
        (Right _) -> return ()
