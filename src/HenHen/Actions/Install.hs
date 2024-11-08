module HenHen.Actions.Install
( install
) where

import HenHen.Config
    ( HenHenConfig(..)
    , readProjectConfig
    , writeProjectConfig
    )
import HenHen.Packager (Packager)
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap

install :: String -> Maybe String -> Packager ()
install name source = do
    config <- readProjectConfig
    let setSource = maybe id (HashMap.insert name) source
    let setDeps   = HashSet.insert name
    writeProjectConfig config
        { configDeps  = setDeps (configDeps config)
        , configFetch = setSource (configFetch config) }
    -- Do nothing else: The dependency will be installed next time we build.
    -- The 'prepare' step handles dependencies on its own.
