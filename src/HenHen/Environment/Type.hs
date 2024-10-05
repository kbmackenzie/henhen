{-# LANGUAGE StrictData #-}

module HenHen.Environment.Type
( Environment
, createEnvironment
) where

import HenHen.Config.Type (HenHenConfig(..), getInstaller)
import Control.Monad.IO.Class (MonadIO(..))
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)
import System.Process (readProcess)
import System.Environment (getEnvironment)
import Data.Maybe (fromMaybe)
import Data.List (unionBy)
import Data.Function (on)
import qualified Data.Text as Text

type Environment = [(String, String)]

data ChickenEnvironment = ChickenEnvironment
    { environmentRoot    :: FilePath
    , environmentRepo    :: FilePath
    , repositoryVariable :: String   }

getChickenVars :: ChickenEnvironment -> [(String, String)]
getChickenVars env =
    [ ("CHICKEN_INSTALL_PREFIX"    , environmentRoot env   )
    , ("CHICKEN_INSTALL_REPOSITORY", environmentRepo env   )
    , ("CHICKEN_REPOSITORY_PATH"   , repositoryVariable env) ]

getChickenEnvironment :: (MonadIO m) => HenHenConfig -> m ChickenEnvironment
getChickenEnvironment config = liftIO $ do
    pwd <- getCurrentDirectory
    let localEnv = pwd </> ".chicken"

    let installer = getInstaller config
    systemRepo <- readProcess (Text.unpack installer) ["-repository"] mempty

    return ChickenEnvironment
        { environmentRoot    = localEnv
        , environmentRepo    = localEnv </> "lib" </> "chicken"
        , repositoryVariable = concat [ localEnv, ":", systemRepo ] }

createEnvironment :: (MonadIO m) => HenHenConfig -> m Environment
createEnvironment config = do
    chickenEnv <- getChickenEnvironment config
    parentEnv  <- liftIO getEnvironment
    let pathVar = fromMaybe mempty $ lookup "PATH" parentEnv

    let newPath    = concat [ environmentRoot chickenEnv </> "bin" , ":" , pathVar ]
    let variables  = (:) ("PATH", newPath) $ getChickenVars chickenEnv
    let processEnv = unionBy ((==) `on` fst) variables parentEnv

    return processEnv
