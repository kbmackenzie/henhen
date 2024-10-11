module HenHen.Actions.Build
( build
) where

import HenHen.Config
    ( HenHenConfig(..)
    , getCompiler
    , getSourcePath
    , Target(..)
    , Meta(..)
    , MetaKey(..)
    , SourceOptions(..)
    , EggOptions(..)
    , getInstaller
    , getTargetMeta
    , getTargetKey
    , isModuleTarget
    )
import HenHen.Environment
    ( Environment
    , localChicken
    , EnvironmentTask(..)
    , runEnvironmentTask
    )
import HenHen.Utils.FilePath (toExecutablePath)
import System.FilePath ((</>), normalise, addExtension)
import System.Directory (createFileLink, createDirectoryIfMissing)
import Data.Maybe (fromMaybe)
import HenHen.Packager (Packager, throwError)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Control.Monad (foldM, foldM_)
import Data.List (singleton)

type GenerateTask a = HenHenConfig -> Meta -> a -> EnvironmentTask

getObjects :: [MetaKey] -> [FilePath]
getObjects = map $ (`addExtension` "o") . getKey

getUses :: [MetaKey] -> [String]
getUses = concatMap $ ("-uses" :) . singleton . getKey

buildSource :: Bool -> GenerateTask SourceOptions
buildSource isModule config meta options = do
    let targetMap = configTargets config

    let name = (getKey . metaKey) meta
    let source = getSourcePath config $ fromMaybe (addExtension name "scm") (sourcePath options)

    let output = "." </> if isModule
        then addExtension name "o"
        else toExecutablePath name

    let filterModules :: [MetaKey] -> [MetaKey]
        filterModules = filter $ maybe False isModuleTarget . (`HashMap.lookup` targetMap)

    let coreFlags = ["-static", "-setup-mode", "-O2"]
    let dependencies = filterModules (metaDeps meta)

    let arguments = if isModule
        then concat
            [ ["-c", "-J", "-regenerate-import-libraries", "-M"]
            , ["-unit", name, "-o", output, source]
            , coreFlags
            , metaOptions meta ]
        else concat
            [ ["-o", output, source]
            , getObjects dependencies
            , getUses dependencies
            , coreFlags
            , metaOptions meta ] 

    let compiler = getCompiler config
    EnvironmentTask
        { taskCommand     = compiler
        , taskArguments   = arguments
        , taskDirectory   = Nothing
        , taskErrorReport = Just . buildFail $ "module " ++ show name
        , afterTask       = Nothing }

buildEgg :: GenerateTask EggOptions
buildEgg config meta options = do
    let name      = (getKey . metaKey) meta
    let installer = getInstaller config
    EnvironmentTask
        { taskCommand     = installer
        , taskArguments   = []
        , taskDirectory   = eggDirectory options
        , taskErrorReport = Just . buildFail $ "egg " ++ show name
        , afterTask       = Nothing }

buildBinary :: GenerateTask SourceOptions
buildBinary config meta options = do
    let task = buildSource True config meta options
    let after :: IO ()
        after = do
            -- Create symlink in '.chicken/bin'!
            let binary  = (getKey . metaKey) meta -- todo: on windows, add '.exe' extension.
            let binPath = localChicken </> "bin"
            createDirectoryIfMissing True binPath
            createFileLink binary (binPath </> binary)
    task { afterTask = Just after }

buildFail :: String -> String -> String
buildFail name message = concat
    [ "Couldn't build ", name, ": ", message ]

buildTarget :: HenHenConfig -> Target -> EnvironmentTask
buildTarget config target = case target of
    (Module meta options)     -> buildSource True config meta options
    (Egg meta options)        -> buildEgg config meta options
    (Executable meta options) -> buildBinary config meta options

build :: HenHenConfig -> Environment -> Packager ()
build config env = do
    let targetMap = configTargets config

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
