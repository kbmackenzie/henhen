{-# LANGUAGE StrictData #-}

module HenHen.Actions.Type
( Action(..)
) where

data Action =
      Build
    | Run       FilePath -- name of binary/script to run
    | Interpret String   -- path to script to run
    | Clean     Bool     -- "should purge?"
    deriving (Show)
