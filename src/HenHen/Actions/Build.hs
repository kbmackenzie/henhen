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
import System.FilePath ((</>), normalise, takeDirectory, replaceExtension)
import System.Directory (createDirectoryIfMissing)
import Data.Maybe (fromMaybe)
import Data.List (singleton)

type BuildFn a = HenHenConfig -> Environment -> Meta -> a -> Packager ()

buildFail :: String -> String -> String
buildFail name message = concat
    [ "Couldn't build ", name, ": ", message ]

buildModule :: BuildFn ModuleOptions
buildModule config env meta options = do
    localEnv <- getLocalChicken

    let name      = (getKey . metaKey) meta
    let sourceDir = maybe id (flip (</>) . normalise) (configSources config)
    let source    = sourceDir $ fromMaybe (name ++ ".scm") (moduleSource options)
    let buildDir  = localEnv </> "build" </> name
    let output    = buildDir </> replaceExtension source "o"
    let useList   = concatMap (("-uses" :) . singleton . getKey) (metaDeps meta)
    let arguments =
            [ "-c", "-J", source
            , "-unit", name
            , "-o"   , output    ]
            ++ useList
            ++ metaOptions meta

    liftIO $ createDirectoryIfMissing True buildDir
    let compiler = getCompiler config
    runEnvironmentTask env EnvironmentTask
        { taskCommand     = compiler
        , taskArguments   = arguments
        , taskDirectory   = Nothing
        , taskErrorReport = Just . buildFail $ "module " ++ show name }

buildEgg :: BuildFn EggOptions
buildEgg config env meta options = do
    let name      = (getKey . metaKey) meta
    let installer = getInstaller config
    runEnvironmentTask env EnvironmentTask
        { taskCommand     = installer
        , taskArguments   = []
        , taskDirectory   = eggDirectory options
        , taskErrorReport = Just . buildFail $ "egg " ++ show name }
