module HenHen.Actions.Build
(
) where

import HenHen.Packager (Packager, liftIO, catchError, throwError)
import HenHen.Config
    ( HenHenConfig(..)
    , getCompiler
    , Target(..)
    , Meta(..)
    , MetaKey(..)
    , ModuleOptions(..)
    , EggOptions(..)
    , ExecutableOptions(..)
    , getInstaller
    )
import HenHen.Environment
    ( Environment
    , getLocalChicken
    , EnvironmentTask(..)
    , runEnvironmentTask
    )
import System.FilePath ((</>), normalise, replaceExtension)
import System.Directory (createDirectoryIfMissing)
import Data.Maybe (fromMaybe)
import Data.List (singleton)

type BuildFn a = HenHenConfig -> Meta -> a -> Packager EnvironmentTask

buildFail :: String -> String -> String
buildFail name message = concat
    [ "Couldn't build ", name, ": ", message ]

buildModule :: BuildFn ModuleOptions
buildModule config meta options = do
    buildDir <- (</> "build") <$> getLocalChicken

    let name      = (getKey . metaKey) meta
    let sourceDir = maybe id (flip (</>) . normalise) (configSources config)
    let source    = sourceDir $ fromMaybe (name ++ ".scm") (moduleSource options)

    let outputDir = buildDir </> name
    let output    = outputDir </> replaceExtension source "o"

    let uses      = concatMap (("-uses" :) . singleton . getKey) (metaDeps meta)
    let includes  = map (("-I" ++) . (buildDir </>) . getKey) (metaDeps meta)
    let arguments = concat
            [ ["-c", "-J", source, "-unit", name, "-o", output]
            , uses
            , includes
            , metaOptions meta ]

    liftIO $ createDirectoryIfMissing True outputDir
    let compiler = getCompiler config
    return EnvironmentTask
        { taskCommand     = compiler
        , taskArguments   = arguments
        , taskDirectory   = Nothing
        , taskErrorReport = Just . buildFail $ "module " ++ show name
        , taskPrepare     = Nothing
        , nextTask        = Nothing }

buildEgg :: BuildFn EggOptions
buildEgg config meta options = do
    let name      = (getKey . metaKey) meta
    let installer = getInstaller config
    return EnvironmentTask
        { taskCommand     = installer
        , taskArguments   = []
        , taskDirectory   = eggDirectory options
        , taskErrorReport = Just . buildFail $ "egg " ++ show name
        , taskPrepare     = Nothing
        , nextTask        = Nothing }
