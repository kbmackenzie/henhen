module HenHen.Actions.Install
( install
) where

import HenHen.Config
    ( HenHenConfig(..)
    , readConfig
    , writeConfig
    )
import HenHen.Packager (Packager)
import qualified Data.HashSet as HashSet

install :: String -> Packager ()
install name = do
    config <- readConfig
    let newDeps = HashSet.insert name (configDeps config)
    writeConfig config { configDeps = newDeps }
    -- Do nothing else: The dependency will be installed next time we build.
    -- The 'prepare' step handles dependencies on its own.