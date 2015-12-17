module Database.HashStatus.Types
( Function (..)
) where

data Function = New
              | UpdateProgress
              | UpdateCtx
              | Clean
              | Done

              | Get
              deriving (Eq, Ord, Show)
