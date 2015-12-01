module Database.FileCache.Types
( Function (..)
) where

data Function = InsertFile
              | RemoveFile
              | LastFileId

              | UpdateName
              | UpdateLocation
              | UpdateSize
              | UpdateAccessTime
              | UpdateModTime
              | UpdateUser
              | UpdateGroup
              | UpdateOther

              | FileExists

              | InsertHash
              | RemoveHash
              | UpdateHash

              | GetUnhashed
              deriving (Eq, Ord)
