module HenHen.Actions.Build
(
) where

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
    , localChicken
    , EnvironmentTask(..)
    , runEnvironmentTask
    )
import System.FilePath ((</>), normalise, replaceExtension)
import System.Directory (createDirectoryIfMissing)
import Data.Maybe (fromMaybe)
import Data.List (singleton, delete)

type GenerateTask a = HenHenConfig -> Meta -> a -> EnvironmentTask

getIncludes :: FilePath -> Meta -> [String]
getIncludes root = map (("-I" ++) . (root </>) . getKey) . metaDeps

getUses :: Meta -> [String]
getUses = concatMap (("-uses" :) . singleton . getKey) . metaDeps

buildModule :: GenerateTask ModuleOptions
buildModule config meta options = do
    let buildDir  = localChicken </> "build"
    let name      = (getKey . metaKey) meta
    let sourceDir = maybe id ((</>) . normalise) (configSources config)
    let source    = sourceDir $ fromMaybe (name ++ ".scm") (moduleSource options)

    let outputDir = buildDir </> name
    let output    = outputDir </> replaceExtension source "o"

    let arguments = concat
            [ ["-c", "-J", source, "-unit", name, "-o", output]
            , getUses meta
            , getIncludes buildDir meta
            , metaOptions meta ]
    let preparations = createDirectoryIfMissing True outputDir

    let compiler = getCompiler config
    EnvironmentTask
        { taskCommand     = compiler
        , taskArguments   = arguments
        , taskDirectory   = Nothing
        , taskErrorReport = Just . buildFail $ "module " ++ show name
        , taskPrepare     = Just preparations
        , nextTask        = Nothing }

buildEgg :: GenerateTask EggOptions
buildEgg config meta options = do
    let name      = (getKey . metaKey) meta
    let installer = getInstaller config
    EnvironmentTask
        { taskCommand     = installer
        , taskArguments   = []
        , taskDirectory   = eggDirectory options
        , taskErrorReport = Just . buildFail $ "egg " ++ show name
        , taskPrepare     = Nothing
        , nextTask        = Nothing }

buildFail :: String -> String -> String
buildFail name message = concat
    [ "Couldn't build ", name, ": ", message ]
