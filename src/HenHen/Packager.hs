{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HenHen.Packager
( Packager
, package
, throwError
, catchError
, liftIO
) where

import Control.Monad.Except (MonadError, ExceptT, runExceptT, throwError, catchError)
import Control.Monad.IO.Class (MonadIO(..))

newtype Packager a = Packager { runPackager :: ExceptT String IO a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadError String
             , MonadIO
             )

package :: Packager a -> IO (Either String a)
package = runExceptT . runPackager
