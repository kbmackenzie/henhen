module HenHen
( Action(..)
, henhen
) where

import HenHen.Packager (package)
import HenHen.Logger (LogLevel)
import HenHen.Actions (Action(..), runAction)

henhen :: Action -> Maybe LogLevel -> IO (Either String ())
henhen = (package .) . runAction
