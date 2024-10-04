{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module HenHen.Environment.Create
(
) where

import HenHen.Environment.Type (Environment(..), getVariableMap)
import Control.Monad.IO.Class (MonadIO(..))
import System.Process.Typed (ProcessConfig, setEnv)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import System.FilePath ((</>))

runInEnvironment :: (MonadIO m) => Environment -> ProcessConfig stdin stdout stderr -> m (ProcessConfig stdin stdout stderr)
runInEnvironment env process = liftIO $ do
    pathVar <- fromMaybe mempty <$> lookupEnv "PATH"
    let variables = getVariableMap env
    let newPath = concat [ environmentRoot env </> "bin" , ":" , pathVar ]

    undefined
