{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HenHen.Packager
( Packager
, package
, throwError
, catchError
, liftEither
, liftIO
) where

import Control.Monad.Except (MonadError, ExceptT, runExceptT, throwError, catchError, liftEither)
import Control.Monad.IO.Class (MonadIO(..))

newtype Packager a = Packager { runPackager :: ExceptT String IO a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadFail
             , MonadError String
             , MonadIO
             )

package :: Packager a -> IO (Either String a)
package = runExceptT . runPackager
