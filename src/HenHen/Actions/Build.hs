module HenHen.Actions.Build
( buildTarget
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
    ( chickenBuild
    , EnvironmentTask(..)
    )
import System.FilePath ((</>), normalise, replaceExtension, dropExtension, addExtension)
import System.Directory (createDirectoryIfMissing)
import Data.Maybe (fromMaybe)
import Data.List (singleton)

type GenerateTask a = HenHenConfig -> Meta -> a -> EnvironmentTask

getIncludes :: Meta -> [String]
getIncludes = map (("-I" ++) . (chickenBuild </>) . getKey) . metaDeps

getUses :: Meta -> [String]
getUses = concatMap (("-uses" :) . singleton . getKey) . metaDeps

getObjects :: Meta -> [String]
getObjects = map ((chickenBuild </>) . (`addExtension` "o") . getKey) . metaDeps

buildModule :: GenerateTask ModuleOptions
buildModule config meta options = do
    let name      = (getKey . metaKey) meta
    let sourceDir = maybe id ((</>) . normalise) (configSources config)
    let source    = sourceDir $ fromMaybe (name ++ ".scm") (moduleSource options)

    let outputDir = chickenBuild </> name
    let output    = outputDir </> replaceExtension source "o"
    let arguments = concat
            [ ["-static", "-c", "-J", source, "-unit", name, "-o", output]
            , getUses meta
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

buildExecutable :: GenerateTask ExecutableOptions
buildExecutable config meta options = do
    let name      = (getKey . metaKey) meta
    let sourceDir = maybe id ((</>) . normalise) (configSources config)
    let source    = sourceDir $ fromMaybe (name ++ ".scm") (executableSource options)

    let outputDir = chickenBuild </> name
    let output    = outputDir </> dropExtension source
    let arguments = concat
            [ ["-static", "-o", output, source]
            , getObjects meta
            , getUses meta
            , metaOptions meta ]
    let preparations = createDirectoryIfMissing True outputDir

    let compiler  = getCompiler config
    EnvironmentTask
        { taskCommand     = compiler
        , taskArguments   = arguments
        , taskDirectory   = Nothing
        , taskErrorReport = Just . buildFail $ "executable " ++ show name
        , taskPrepare     = Just preparations
        , nextTask        = Nothing }

buildFail :: String -> String -> String
buildFail name message = concat
    [ "Couldn't build ", name, ": ", message ]

buildTarget :: HenHenConfig -> Target -> EnvironmentTask
buildTarget config target = case target of
    (Module meta options)     -> buildModule config meta options
    (Egg meta options)        -> buildEgg config meta options
    (Executable meta options) -> buildExecutable config meta options
