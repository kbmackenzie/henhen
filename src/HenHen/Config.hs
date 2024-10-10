module HenHen.Config
( HenHenConfig(..)
, Aliases(..)
, getInstaller
, getCompiler
, getInterpreter
, getStatus
, getUninstaller
, Target(..)
, Meta(..)
, MetaKey(..)
, SourceOptions(..)
, EggOptions(..)
, getTargetKey
, getTargetMap
) where

import HenHen.Config.Type
    ( HenHenConfig(..)
    , Aliases(..)
    , getInstaller
    , getCompiler
    , getInterpreter
    , getStatus
    , getUninstaller
    )
import HenHen.Config.Target
    ( Target(..)
    , Meta(..)
    , MetaKey(..)
    , SourceOptions(..)
    , EggOptions(..)
    , getTargetKey
    , getTargetMap
    )
