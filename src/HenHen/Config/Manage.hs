module HenHen.Config.Manage
( configPath
, readConfig
, writeConfig
) where

import HenHen.Config.Type (HenHenConfig(..), configFieldOrder)
import HenHen.Packager (Packager, liftEither)
import HenHen.Utils.IO (readFileSafe, writeFileSafe)
import HenHen.Utils.Yaml (readYaml, prettyYaml)

configPath :: FilePath
configPath = "henhen.yaml"

readConfig :: Packager HenHenConfig
readConfig = do
    content <- readFileSafe configPath
    liftEither $ readYaml content

writeConfig :: HenHenConfig -> Packager ()
writeConfig config = do
    let yaml = prettyYaml configFieldOrder config
    writeFileSafe configPath yaml
