module HenHen.Actions.Build
( build
, buildAll
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
    )
import HenHen.Environment
    ( Environment
    , localBuild
    , localChickenBin
    , EnvironmentTask(..)
    , runEnvironmentTask
    )
import HenHen.Utils.FilePath (toExecutablePath)
import HenHen.Utils.IO (fileLink)
import System.FilePath ((</>), addExtension)
import Data.Maybe (fromMaybe, mapMaybe)
import HenHen.Packager (Packager)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Control.Monad (foldM, foldM_)

type GenerateTask a = HenHenConfig -> Meta -> a -> EnvironmentTask

buildBinary :: GenerateTask SourceOptions
buildBinary config meta options = do
    let name   = (getKey . metaKey) meta
    let source = fromMaybe (addExtension name "scm") (sourcePath options)
    let binary = toExecutablePath name

    let coreFlags = ["-static", "-setup-mode", "-O2"]
    let arguments = concat
            [ ["-o", binary, source]
            , coreFlags
            , metaOptions meta ] 

    let after :: Packager ()
        after = do
            -- Create symlink in '<local-chicken>/bin'!
            let from = localBuild </> binary
            let to   = localChickenBin </> binary
            fileLink from to

    let compiler = getCompiler config
    EnvironmentTask
        { taskCommand     = compiler
        , taskArguments   = arguments
        , taskDirectory   = Just localBuild
        , taskErrorReport = Just . buildFail $ "module " ++ show name
        , afterTask       = Just after }

buildEgg :: GenerateTask EggOptions
buildEgg config meta options = do
    let name      = (getKey . metaKey) meta
    let directory = maybe localBuild (localBuild </>) (eggDirectory options)
    let installer = getInstaller config
    EnvironmentTask
        { taskCommand     = installer
        , taskArguments   = []
        , taskDirectory   = Just directory
        , taskErrorReport = Just . buildFail $ "egg " ++ show name
        , afterTask       = Nothing }

buildFail :: String -> String -> String
buildFail name message = concat
    [ "Couldn't build ", name, ": ", message ]

build :: HenHenConfig -> Target -> EnvironmentTask
build config target = case target of
    (Egg meta options)        -> buildEgg config meta options
    (Executable meta options) -> buildBinary config meta options

buildAll :: HenHenConfig -> Environment -> Packager ()
buildAll config env = do
    let targetMap = configTargets config

    let getDependencies :: Target -> [Target]
        getDependencies target = do
            -- Note: Dependencies that aren't targets are handled elsewhere.
            -- Because of this, we simply ignore them here.
            let keys = (metaDeps . getTargetMeta) target
            mapMaybe (`HashMap.lookup` targetMap) keys

    let deepBuild :: HashSet MetaKey -> Target -> Packager (HashSet MetaKey)
        deepBuild visited target = do
            let self = getTargetKey target
            if HashSet.member self visited then return visited else do
                -- Build all dependencies recursively, in order, storing the new 'visited' set.
                let visitedSelf = HashSet.insert self visited
                let buildDependencies = foldM deepBuild visitedSelf (getDependencies target)

                visitedDependencies <- buildDependencies
                runEnvironmentTask env (build config target)
                return visitedDependencies

    foldM_ deepBuild mempty $ configTargets config
