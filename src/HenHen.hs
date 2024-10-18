module HenHen
( henhen
) where

import HenHen.Packager (package)
import HenHen.Actions (Action(..), runAction)
import Control.Monad.IO.Class (MonadIO(..))

henhen :: (MonadIO m) => Action -> m (Either String ())
henhen = liftIO . package . runAction
