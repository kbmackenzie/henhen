module HenHen
( henhen
) where

import HenHen.Packager (package)
import HenHen.Actions (Action(..), runAction)

henhen :: Action -> IO (Either String ())
henhen = package . runAction
