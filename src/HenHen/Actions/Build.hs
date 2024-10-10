module HenHen.Actions.Build
( build
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
    , getTargetMeta
    , getTargetKey
    , getTargetMap
    )
import HenHen.Environment
    ( chickenBuild
    , EnvironmentTask(..)
    , runEnvironmentTask
    , Environment
    )
import System.FilePath ((</>), normalise, addExtension, replaceExtension, dropExtension)
import System.Directory (createDirectoryIfMissing)
import Data.Maybe (fromMaybe)
import HenHen.Packager (Packager, throwError)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Control.Monad (foldM, foldM_)

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
    let source = sourceDir $ fromMaybe (addExtension name "scm") (sourcePath options)

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

build :: HenHenConfig -> Environment -> Packager ()
build config env = do
    let targetMap = getTargetMap (configTargets config)

    let getTarget :: MetaKey -> Packager Target
        getTarget key = case HashMap.lookup key targetMap of
            (Just target) -> return target
            Nothing       -> throwError ("Target doesn't exist: " ++ (show . getKey) key)

    let getDependencies :: Target -> Packager [Target]
        getDependencies target = do
            let keys = (metaDeps . getTargetMeta) target
            mapM getTarget keys

    let deepBuild :: HashSet MetaKey -> Target -> Packager (HashSet MetaKey)
        deepBuild visited target = do
            let self = getTargetKey target
            if HashSet.member self visited then return visited else do
                -- Build all dependencies recursively, in order, storing the new 'visited' set:
                let visitedSelf = HashSet.insert self visited
                let buildDependencies = foldM deepBuild visitedSelf =<< getDependencies target

                visitedDependencies <- buildDependencies
                runEnvironmentTask env (buildTarget config target)
                return visitedDependencies

    foldM_ deepBuild mempty $ configTargets config
