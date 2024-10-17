module HenHen.Actions.Type
( Action(..)
) where

data Action = Build | Interpret | Run | Clean
    deriving (Eq, Ord, Show, Enum, Bounded)
