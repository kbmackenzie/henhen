{-# LANGUAGE StrictData #-}

module HenHen.Config.Target
( Target(..)
, Meta(..)
, MetaKey(..)
, ModuleOptions(..)
, EggOptions(..)
, ExecutableOptions(..)
) where

data Meta = Meta
    { metaKey   :: MetaKey
    , metaDeps  :: [MetaKey] }

newtype MetaKey = Key { getKey :: MetaKey }

data Target =
      Module     Meta ModuleOptions
    | Egg        Meta EggOptions
    | Executable Meta ExecutableOptions

data ModuleOptions = ModuleOptions
    { moduleSource   :: FilePath
    , moduleIncludes :: FilePath }

newtype EggOptions = EggOptions
    { eggDirectory   :: Maybe FilePath }

data ExecutableOptions = ExecutableOptions
    { executableName   :: String
    , executableSource :: FilePath
    , executableStatic :: Bool     }
