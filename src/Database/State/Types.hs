module Database.State.Types
( Function (..)
) where

data Function = Insert
              | LastState
              | MaxId
              deriving (Eq, Ord, Show)
