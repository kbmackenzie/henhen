module HenHen.Environment
( Environment
, createEnvironment
, localChicken
, localBuild
, EnvironmentTask(..)
, runEnvironmentTask
) where

import HenHen.Environment.Folders (localChicken, localBuild)
import HenHen.Environment.Type (Environment, createEnvironment)
import HenHen.Environment.Task (EnvironmentTask(..), runEnvironmentTask)
