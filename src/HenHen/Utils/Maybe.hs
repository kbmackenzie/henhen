module HenHen.Utils.Maybe
( optional
) where

import Data.Maybe (fromMaybe)

optional :: (Functor f) => a -> f (Maybe a) -> f a
optional = fmap . fromMaybe
