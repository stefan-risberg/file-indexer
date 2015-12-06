module Database.FileCache.Types
( Function (..)
) where

data Function = InsertFile
              | RemoveFile
              | LastFileId

              | FileExists

              | UpdateName
              | UpdateLocation
              | UpdateSize
              | UpdateAccessTime
              | UpdateModTime
              | UpdateUser
              | UpdateGroup
              | UpdateOther

              | InsertHash
              | RemoveHash
              | UpdateHash

              | GetUnhashed
              deriving (Eq, Ord)
