module HenHen.Actions.Uninstall
( uninstall
) where

import HenHen.Config
    ( HenHenConfig(..)
    , readProjectConfig
    , writeProjectConfig
    )
import HenHen.Packager (Packager)
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap

uninstall :: String -> Bool -> Packager ()
uninstall name cleanFetchMap = do
    config <- readProjectConfig
    let removeDep = HashSet.delete name
    let setFetch  = if cleanFetchMap then HashMap.delete name else id
    writeProjectConfig config
        { configDeps  = removeDep (configDeps config)
        , configFetch = setFetch (configFetch config) }
    -- Do nothing else. The 'prepare' step handles dependencies on its own.
    -- Consider: Adding a 'henhen prune' option to remove unneeded dependencies?
