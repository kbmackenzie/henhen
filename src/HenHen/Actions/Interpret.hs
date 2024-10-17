module HenHen.Actions.Interpret
( interpret
) where

import HenHen.Environment
    ( EnvironmentTask(..)
    )
import HenHen.Packager (throwError)
import HenHen.Config
    ( HenHenConfig(..)
    , getInterpreter
    , getSourcePath
    , Target(..)
    , Meta(..)
    , MetaKey(..)
    , SourceOptions(..)
    )
import qualified Data.HashMap.Strict as HashMap
import System.FilePath (addExtension)
import Data.Maybe (fromMaybe)

eggFail :: String
eggFail = "HenHen cannot interpret eggs."

getSource :: HenHenConfig -> Target -> Either String FilePath
getSource config target = case target of
    (Executable meta options) -> do
        let name = (getKey . metaKey) meta
        let path = fromMaybe (addExtension name "scm") (sourcePath options)
        return . getSourcePath config $ path
    (Egg meta _) -> let name = (getKey . metaKey) meta
        in throwError . concat $ ["Cannot interpret target ", show name, ": ", eggFail]

runInterpreter :: HenHenConfig -> FilePath -> EnvironmentTask
runInterpreter config path = do
    let interpreter = getInterpreter config
    EnvironmentTask
        { taskCommand     = interpreter
        , taskArguments   = ["-s", path, "-b"]
        , taskDirectory   = Nothing
        , taskErrorReport = Nothing
        , afterTask       = Nothing }

interpret :: HenHenConfig -> String -> Either String EnvironmentTask
interpret config name = do
    let maybeTarget = HashMap.lookup (MetaKey name) (configTargets config)
    case maybeTarget of
        (Just target) -> do
            source <- getSource config target
            return $ runInterpreter config source
        Nothing -> throwError ("No target found with name " ++ show name)
