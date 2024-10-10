module HenHen.Actions.Build
( buildTarget
) where

import HenHen.Config
    ( HenHenConfig(..)
    , getCompiler
    , Target(..)
    , Meta(..)
    , MetaKey(..)
    , SourceOptions(..)
    , EggOptions(..)
    , getInstaller
    )
import HenHen.Environment
    ( chickenBuild
    , EnvironmentTask(..)
    )
import System.FilePath ((</>), normalise, replaceExtension, dropExtension)
import System.Directory (createDirectoryIfMissing)
import Data.Maybe (fromMaybe)

type GenerateTask a = HenHenConfig -> Meta -> a -> EnvironmentTask

getIncludes :: Meta -> [String]
getIncludes meta = do
    let dependencies = metaDeps meta
    let includes flag = map $ (flag ++) . (chickenBuild </>) . getKey
    includes "-I " dependencies ++ includes "-C -I" dependencies

buildSource :: Bool -> GenerateTask SourceOptions
buildSource isModule config meta options = do
    let name = (getKey . metaKey) meta
    let sourceDir = maybe id ((</>) . normalise) (configSources config)
    let source = sourceDir $ fromMaybe (name ++ ".scm") (sourcePath options)

    let outputDir = chickenBuild </> name
    let output = outputDir </> if isModule
        then replaceExtension source "o"
        else dropExtension source

    let specialArgs = if isModule
        then ["-c", "-J", "-regenerate-import-libraries", "-M"]
        else mempty

    let arguments = concat
            [ specialArgs
            , [source, "-unit", name, "-o", output]
            , getIncludes meta
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

buildTarget :: HenHenConfig -> Target -> EnvironmentTask
buildTarget config target = case target of
    (Module meta options)     -> buildSource True config meta options
    (Egg meta options)        -> buildEgg config meta options
    (Executable meta options) -> buildSource False config meta options
