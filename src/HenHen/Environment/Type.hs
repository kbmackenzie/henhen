{-# LANGUAGE StrictData #-}

module HenHen.Environment.Type
( Environment
, getLocalChicken
, createEnvironment
) where

import HenHen.Config (HenHenConfig(..), getInstaller)
import HenHen.Packager (Packager, liftEither)
import HenHen.Environment.Folders (localChicken)
import Data.List (unionBy)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (MonadIO(..))
import System.FilePath ((</>))
import System.Directory (canonicalizePath)
import System.Process (readProcess)
import System.Environment (getEnvironment)
import Control.Exception (IOException, catch)
import Data.Bifunctor (first)

type Environment = [(String, String)]

data ChickenEnvironment = ChickenEnvironment
    { environmentRoot    :: FilePath
    , environmentRepo    :: FilePath
    , environmentCache   :: FilePath
    , repositoryVariable :: String   }
    deriving (Show)

getLocalChicken :: (MonadIO m) => m FilePath
getLocalChicken = liftIO (canonicalizePath localChicken)

getSystemChicken :: (MonadIO m) => String -> m (Either String FilePath)
getSystemChicken installer = do
    let getRepository :: IO (Either String FilePath)
        getRepository = do
            let repository = readProcess installer ["-repository"] mempty
            fmap Right repository `catch` \e -> do
                let errorMessage = show (e :: IOException)
                return (Left errorMessage)

    let getFirstLine :: String -> Either String String
        getFirstLine str = case lines str of
            (x:_) -> Right x
            []    -> Left "Expected lines, got empty string!"

    let addErrorContext :: Either String String -> Either String String
        addErrorContext = first ("Couldn't get CHICKEN system repository: " ++)

    repository <- liftIO getRepository
    (return . addErrorContext) (repository >>= getFirstLine)

getChickenVars :: ChickenEnvironment -> [(String, String)]
getChickenVars env =
    [ ("CHICKEN_INSTALL_PREFIX"    , environmentRoot env   )
    , ("CHICKEN_INSTALL_REPOSITORY", environmentRepo env   )
    , ("CHICKEN_EGG_CACHE"         , environmentCache env  )
    , ("CHICKEN_REPOSITORY_PATH"   , repositoryVariable env) ]

getChickenEnvironment :: HenHenConfig -> Packager ChickenEnvironment
getChickenEnvironment config = do
    localEnv <- getLocalChicken
    let installer = getInstaller config
    systemRepo <- getSystemChicken installer >>= liftEither

    return ChickenEnvironment
        { environmentRoot    = localEnv
        , environmentRepo    = localEnv </> "lib" </> "chicken"
        , environmentCache   = localEnv </> "cache"
        , repositoryVariable = concat [ localEnv, ":", systemRepo ] }

createEnvironment :: HenHenConfig -> Packager Environment
createEnvironment config = do
    chickenEnv <- getChickenEnvironment config
    parentEnv  <- liftIO getEnvironment
    let pathVar = fromMaybe mempty $ lookup "PATH" parentEnv

    let newPath    = concat [ environmentRoot chickenEnv </> "bin" , ":" , pathVar ]
    let variables  = (:) ("PATH", newPath) $ getChickenVars chickenEnv
    let processEnv = unionBy ((==) `on` fst) variables parentEnv

    liftIO $ do
        print chickenEnv
        print processEnv
    return processEnv
