{-# LANGUAGE LambdaCase #-}

module HenHen.CLI.Run
( run
) where

import HenHen (henhen)
import HenHen.CLI.Options (getAction)
import System.IO (hPutStrLn, stderr)

run :: IO ()
run = getAction >>= henhen >>= \case
    (Left e)  -> hPutStrLn stderr ("[henhen] " ++ e)
    (Right _) -> return ()
