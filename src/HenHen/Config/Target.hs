{-# LANGUAGE StrictData #-}

module HenHen.Config.Target
( Target(..)
, TargetName(..)
, TargetType(..)
) where

data TargetType =
      Library
    | Executable
    | TestSuite
    deriving (Eq, Ord, Bounded, Enum, Show)

newtype TargetName = TargetName
    { getTargetName :: String }
    deriving (Show)

data Target = Target
    { targetName    :: TargetName
    , targetType    :: TargetType
    , targetSources :: [FilePath]
    , targetDeps    :: [TargetName]
    }
    deriving (Show)
