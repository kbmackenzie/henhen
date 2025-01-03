{-# LANGUAGE StrictData #-}

module HenHen.Actions.Type
( Action(..)
) where

data Action =
      Build
    | Run       String [String]         -- name of binary/script to run + argument list
    | Init      (Maybe String)          -- name of project
    | Install   String (Maybe String)   -- name of dep to install + optional source url
    | Uninstall String Bool             -- name of dep to uninstall + "should clean fetch map?"
    | Interpret FilePath                -- path to script to run
    | REPL
    | Copy      String FilePath         -- name of binary target and destination to copy to
    | Clean     Bool                    -- "should purge?"
    deriving (Show)
