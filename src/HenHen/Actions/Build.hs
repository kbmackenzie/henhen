{-# LANGUAGE TupleSections #-}

module HenHen.Actions.Build
( build
, buildAll
) where

import HenHen.Config
    ( HenHenConfig(..)
    , getCompiler
    , Target(..)
    , TargetKey(..)
    , TargetMeta(..)
    , SourceOptions(..)
    , EggOptions(..)
    , getInstaller
    , getTargetMeta
    )
import HenHen.Environment
    ( Environment
    , localBuild
    , localChickenBin
    , EnvironmentTask(..)
    , runEnvironmentTask
    )
import HenHen.Utils.FilePath (toExecutablePath)
import HenHen.Utils.IO (createFileLinkSafe)
import System.FilePath ((</>), addExtension)
import Data.Maybe (fromMaybe, mapMaybe)
import HenHen.Packager (Packager)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Control.Monad (foldM, foldM_)

type GenerateTask a = HenHenConfig -> TargetKey -> TargetMeta -> a -> EnvironmentTask

buildBinary :: GenerateTask SourceOptions
buildBinary config key meta options = do
    let name   = getKey key
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
            createFileLinkSafe True from to

    let compiler = getCompiler config
    EnvironmentTask
        { taskCommand     = compiler
        , taskArguments   = arguments
        , taskDirectory   = Just localBuild
        , taskErrorReport = Just . buildFail $ "module " ++ show name
        , afterTask       = Just after }

buildEgg :: GenerateTask EggOptions
buildEgg config key meta options = do
    let name      = getKey key
    let directory = maybe localBuild (localBuild </>) (eggDirectory options)
    let installer = getInstaller config
    EnvironmentTask
        { taskCommand     = installer
        , taskArguments   = metaOptions meta
        , taskDirectory   = Just directory
        , taskErrorReport = Just . buildFail $ "egg " ++ show name
        , afterTask       = Nothing }

buildFail :: String -> String -> String
buildFail name message = concat
    [ "Couldn't build ", name, ": ", message ]

build :: HenHenConfig -> (TargetKey, Target) -> EnvironmentTask
build config (key, target) = case target of
    (Egg meta options)        -> buildEgg config key meta options
    (Executable meta options) -> buildBinary config key meta options

buildAll :: HenHenConfig -> Environment -> Packager ()
buildAll config env = do
    let targetMap = configTargets config

    let getDependencies :: Target -> [(TargetKey, Target)]
        getDependencies target = do
            -- Note: Dependencies that aren't targets are handled elsewhere.
            -- Because of this, we simply ignore them here.
            let find :: TargetKey -> Maybe (TargetKey, Target)
                find key = (key,) <$> HashMap.lookup key targetMap

            let keys = (metaDeps . getTargetMeta) target
            mapMaybe find keys

    let deepBuild :: HashSet TargetKey -> (TargetKey, Target) -> Packager (HashSet TargetKey)
        deepBuild visited (self, target) = do
            if HashSet.member self visited then return visited else do
                -- Build all dependencies recursively, in order, storing the new 'visited' set.
                let visitedSelf = HashSet.insert self visited
                let buildDependencies = foldM deepBuild visitedSelf (getDependencies target)

                visitedDependencies <- buildDependencies
                runEnvironmentTask config env $ build config (self, target)
                return visitedDependencies

    foldM_ deepBuild mempty ((HashMap.toList . configTargets) config)
