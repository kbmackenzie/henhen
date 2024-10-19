{-# LANGUAGE StrictData #-}

module HenHen.Actions.Type
( Action(..)
) where

data Action =
      Build
    | Run       String [String]     -- name of binary/script to run + argument list
    | Init      String              -- name of project
    | Interpret FilePath            -- path to script to run
    | Install   String              -- name of dep to install
    | Clean     Bool                -- "should purge?"
    deriving (Show)
