module HenHen.Config
( HenHenConfig(..)
, Aliases(..)
, getInstaller
, getCompiler
, getInterpreter
, getStatus
, getUninstaller
, getSourcePath
, Target(..)
, Meta(..)
, TargetKey(..)
, SourceOptions(..)
, EggOptions(..)
, getTargetMeta
, configPath
, readConfig
, writeConfig
, writeAsConfig
, hasConfig
) where

import HenHen.Config.Type
    ( HenHenConfig(..)
    , Aliases(..)
    , getInstaller
    , getCompiler
    , getInterpreter
    , getStatus
    , getUninstaller
    , getSourcePath
    )
import HenHen.Config.Target
    ( Target(..)
    , Meta(..)
    , TargetKey(..)
    , SourceOptions(..)
    , EggOptions(..)
    , getTargetMeta
    )
import HenHen.Config.Manage
    ( configPath
    , readConfig
    , writeConfig
    , writeAsConfig
    , hasConfig
    )
