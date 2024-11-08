module HenHen.Actions.Init
( initialize
) where

import HenHen.Config (HenHenConfig(..), writeProjectConfig)
import HenHen.Packager (Packager)
import HenHen.Environment (localChicken)
import HenHen.Utils.IO (writeFileSafe)
import HenHen.Utils.String (stringToByteString)
import Data.ByteString (ByteString)

defaultConfig :: String -> HenHenConfig
defaultConfig name = HenHenConfig
    { configName      = Just name
    , configSources   = ["*.scm", "*.egg"]
    , configDataFiles = []
    , configSourceDir = Nothing
    , configDeps      = mempty
    , configFetch     = mempty
    , configScripts   = mempty
    , configAliases   = Nothing
    , configTargets   = mempty
    , configLogLevel  = Nothing }

gitIgnore :: ByteString
gitIgnore = stringToByteString (localChicken ++ "\n")

initialize :: String -> Packager ()
initialize name = do
    writeProjectConfig (defaultConfig name)
    writeFileSafe ".gitignore" gitIgnore
