{-# LANGUAGE StrictData #-}

module HenHen.Environment.Type
( Environment(..)
, getVariableMap
, getEnvironment
) where

import HenHen.Config.Type (HenHenConfig(..), getInstaller)
import Control.Monad.IO.Class (MonadIO(..))
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)
import System.Process (readProcess)
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy.Char8 as Char8

data Environment = Environment
    { environmentRoot    :: FilePath
    , environmentRepo    :: FilePath
    , repositoryVariable :: String   }
    deriving (Eq, Show)

getVariableMap :: Environment -> [(String, String)]
getVariableMap env =
    [ ("CHICKEN_INSTALL_PREFIX"    , environmentRoot env   )
    , ("CHICKEN_INSTALL_REPOSITORY", environmentRepo env   )
    , ("CHICKEN_REPOSITORY_PATH"   , repositoryVariable env) ]

getEnvironment :: (MonadIO m) => HenHenConfig -> m Environment
getEnvironment config = liftIO $ do
    pwd <- getCurrentDirectory
    let localEnv = pwd </> ".chicken"

    let installer = getInstaller config
    systemRepo <- fmap Char8.unpack $ -- Assumes ASCII.
        readProcessStdout_ $ proc (Text.unpack installer) ["-repository"]

    return Environment
        { environmentRoot    = localEnv
        , environmentRepo    = localEnv </> "lib" </> "chicken"
        , repositoryVariable = concat [ localEnv, ":", systemRepo ] }
