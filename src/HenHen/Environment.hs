module HenHen.Environment
( Environment
, createEnvironment
, localChicken
, localBuild
, localChickenBin
, localDependencies
, EnvironmentTask(..)
, runEnvironmentTask
) where

import HenHen.Environment.Folders
    ( localChicken
    , localBuild
    , localChickenBin
    , localDependencies
    )
import HenHen.Environment.Type (Environment, createEnvironment)
import HenHen.Environment.Task (EnvironmentTask(..), runEnvironmentTask)
