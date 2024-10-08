{-# LANGUAGE StrictData #-}

module HenHen.Environment.Type
( Environment
, getLocalChicken
, createEnvironment
) where

import HenHen.Config.Type (HenHenConfig(..), getInstaller)
import HenHen.Packager (Packager, liftEither)
import Data.List (unionBy)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (MonadIO(..))
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)
import System.Process (readProcess)
import System.Environment (getEnvironment)
import Control.Exception (IOException, catch)

type Environment = [(String, String)]

data ChickenEnvironment = ChickenEnvironment
    { environmentRoot    :: FilePath
    , environmentRepo    :: FilePath
    , repositoryVariable :: String   }

getLocalChicken :: (MonadIO m) => m FilePath
getLocalChicken = (</> ".chicken") <$> liftIO getCurrentDirectory

getSystemChicken :: (MonadIO m) => String -> m (Either String FilePath)
getSystemChicken installer = liftIO $ do
    let getter = readProcess installer ["-repository"] mempty
    fmap Right getter `catch` \e -> do
        let errorMessage = show (e :: IOException)
        return (Left errorMessage)

getChickenVars :: ChickenEnvironment -> [(String, String)]
getChickenVars env =
    [ ("CHICKEN_INSTALL_PREFIX"    , environmentRoot env   )
    , ("CHICKEN_INSTALL_REPOSITORY", environmentRepo env   )
    , ("CHICKEN_REPOSITORY_PATH"   , repositoryVariable env) ]

getChickenEnvironment :: HenHenConfig -> Packager ChickenEnvironment
getChickenEnvironment config = do
    localEnv <- getLocalChicken
    let installer = getInstaller config
    systemRepo <- getSystemChicken installer >>= liftEither

    return ChickenEnvironment
        { environmentRoot    = localEnv
        , environmentRepo    = localEnv </> "lib" </> "chicken"
        , repositoryVariable = concat [ localEnv, ":", systemRepo ] }

createEnvironment :: HenHenConfig -> Packager Environment
createEnvironment config = do
    chickenEnv <- getChickenEnvironment config
    parentEnv  <- liftIO getEnvironment
    let pathVar = fromMaybe mempty $ lookup "PATH" parentEnv

    let newPath    = concat [ environmentRoot chickenEnv </> "bin" , ":" , pathVar ]
    let variables  = (:) ("PATH", newPath) $ getChickenVars chickenEnv
    let processEnv = unionBy ((==) `on` fst) variables parentEnv

    return processEnv
