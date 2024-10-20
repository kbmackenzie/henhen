{-# LANGUAGE StrictData #-}

module HenHen.Environment.Type
( Environment
, getLocalChicken
, createEnvironment
) where

import HenHen.Config (HenHenConfig(..), getInstaller)
import HenHen.Packager (Packager, liftEither)
import HenHen.Environment.Folders (localChicken)
import Data.List (unionBy, singleton)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (MonadIO(..))
import System.FilePath ((</>), searchPathSeparator)
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
    let repositories = concat [localEnv, singleton searchPathSeparator, systemRepo]

    return ChickenEnvironment
        { environmentRoot    = localEnv
        , environmentRepo    = localEnv </> "lib" </> "chicken"
        , environmentCache   = localEnv </> "cache"
        , repositoryVariable = repositories }

createEnvironment :: HenHenConfig -> Packager Environment
createEnvironment config = do
    chickenEnv <- getChickenEnvironment config
    parentEnv  <- liftIO getEnvironment
    let searchPath = fromMaybe mempty $ lookup "PATH" parentEnv

    let newSearchPath = concat
            [ environmentRoot chickenEnv </> "bin"
            , singleton searchPathSeparator
            , searchPath ]
    let variables  = (:) ("PATH", newSearchPath) $ getChickenVars chickenEnv
    let processEnv = unionBy ((==) `on` fst) variables parentEnv

    liftIO $ do
        print chickenEnv
        print processEnv
    return processEnv
