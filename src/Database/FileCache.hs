{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Database.FileCache
( sql
, create

, insertFile
, insertFile'
, removeFile
, lastFileId
, lastFileId'

, fileExists

, updateName
, updateLocation
, updateSize
, updateAccessTime
, updateModTime
, updateUser
, updateGroup
, updateOther

, insertHash
, removeHash

, updateHash
, updateHash'
, getUnhashed
) where

import Database.FileCache.Types
import Database.Types hiding (SqlConnT (conn, st))
import qualified Database.Types as DB
import Database.HDBC
import Database.HDBC.Sqlite3 (Connection)
import Data.Map.Strict ((!), Map)
import Data.Maybe (fromMaybe)
import Data.LargeWord (Word128)
import Data.Convertible.Base
import Data.Text (Text, pack)
import Data.Time.Clock (UTCTime)

import Control.Monad (void, liftM)
import Control.Monad.IO.Class
import Numeric
import Types.File
import Prelude hiding (id)
import qualified Types.File as F
import qualified Types.Permission as P

import System.FilePath (splitFileName, (</>))

import qualified Data.Conduit as C
import Data.Int (Int64)

sql :: [(Function, String)]
sql = [ (InsertFile, "INSERT INTO files (id, name, location, size, access_time, mod_time, user_per, group_per, other_per) \n\
                     \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?);")
      , (RemoveFile, " DELETE FROM files \n\
                     \  WHERE id=?; \n\
                     \ DELETE FROM hashes \n\
                     \  WHERE file_id=?;")
      , (LastFileId, "SELECT MAX(id) FROM files;")

      , (FileExists, "SELECT COUNT(f.id) \n\
                     \  FROM files AS f \n\
                     \ WHERE f.location = ? \n\
                     \   AND f.name = ?;")

      , (UpdateName, "UPDATE files \n\
                     \   SET name=? \n\
                     \ WHERE id=?;")
      , (UpdateLocation, "UPDATE files \n\
                         \   SET location=? \n\
                         \ WHERE id=?;")
      , (UpdateSize, "UPDATE files \n\
                     \   SET size=? \n\
                     \ WHERE id=?;")
      , (UpdateAccessTime, "UPDATE files \n\
                           \   SET access_time=? \n\
                           \ WHERE id=?;")
      , (UpdateModTime, "UPDATE files \n\
                        \   SET mod_time=? \n\
                        \ WHERE id=?;")
      , (UpdateUser, "UPDATE files \n\
                     \   SET user_per=? \n\
                     \ WHERE id=?;")
      , (UpdateGroup, "UPDATE files \n\
                      \   SET group_per=? \n\
                      \ WHERE id=?;")
      , (UpdateOther, "UPDATE files \n\
                      \   SET other_per=? \n\
                      \ WHERE id=?;")

      , (InsertHash, "INSERT INTO hashes (file_id, hash) \n\
                     \SELECT f.id, ? \n\
                     \  FROM files AS f \n\
                     \ WHERE f.id=?;")
      , (RemoveHash, "DELETE FROM hashes \n\
                     \ WHERE file_id=?;")
      , (UpdateHash, "UPDATE hashes \n\
                     \   SET hash=? \n\
                     \ WHERE file_id=?;")
      , (GetUnhashed, "SELECT f.id \n\
                      \      ,f.name \n\
                      \      ,f.location \n\
                      \      ,f.size \n\
                      \      ,f.access_time \n\
                      \      ,f.mod_time \n\
                      \      ,f.user_per \n\
                      \      ,f.group_per \n\
                      \      ,f.other_per \n\
                      \  FROM files AS f \n\
                      \ WHERE f.id NOT IN (SELECT h.file_id \n\
                      \                    FROM hashes AS h);")
      ]

create :: Connection
       -> IO ()
create c = do
    void $ run c "CREATE TABLE IF NOT EXISTS files \n\
                 \ ( id INTEGER \n\
                 \ , name TEXT NOT NULL \n\
                 \ , location TEXT NOT NULL \n\
                 \ , size TEXT NOT NULL \n\
                 \ , access_time TEXT NOT NULL \n\
                 \ , mod_time TEXT NOT NULL \n\
                 \ , user_per INTEGER NOT NULL \n\
                 \ , group_per INTEGER NOT NULL \n\
                 \ , other_per INTEGER NOT NULL \n\
                 \ , PRIMARY KEY (id)\n\
                 \ );" []
    void $ run c "CREATE TABLE IF NOT EXISTS hashes \n\
                 \     ( file_id INTEGER \n\
                 \     , hash TEXT NOT NULL \n\
                 \     , PRIMARY KEY (file_id) \n\
                 \     , FOREIGN KEY (file_id) REFERENCES files(id) \n\
                 \     );" []

-- | Add a file that has an id already.
insertFile :: SqlConn -- ^ DB connection.
           -> File -- ^ File to insert.
           -> IO ()
insertFile c file =
    let ins = DB.st c ! SqlFileCashe InsertFile
        (d, f) = splitFileName (F.path file)
        param     = [ toSql $ F.id file
                    , toSql f
                    , toSql d
                    , toSql $ F.size file
                    , toSql $ F.accessTime file
                    , toSql $ F.modTime file
                    , toSql $ F.user file
                    , toSql $ F.group file
                    , toSql $ F.other file
                    ]
    in void $! execute ins param

-- | Add new file.
insertFile' :: SqlConn -- ^ DB connection.
            -> File    -- ^ File to insert.
            -> IO File -- ^ File with update id field.
insertFile' c file =
    liftM (+1) (lastFileId' c)
    >>= \i -> insertFile c (file { id = i }) >> (return $! file { id = i })

-- | Remove file.
removeFile :: SqlConn -- ^ DB connection.
           -> Int     -- ^ Id of file to remove.
           -> IO ()
removeFile c i =
    let st = DB.st c ! SqlFileCashe RemoveFile
        sI = toSql i
        param = [ sI, sI ]
    in void $! execute st param

-- | Get last file id. If Nothing the file table is empty.
lastFileId :: SqlConn        -- ^ DB connection.
           -> IO (Maybe Int) -- ^ Last id.
lastFileId c =
    let st = DB.st c ! SqlFileCashe LastFileId
        conv r = Just $ fromSql (r ! "MAX(id)")
    in execute st [] >> fetchRowMap st
       >>= \i -> return $! maybe Nothing conv i

-- | Get last file id. Same as 'lastFileId' except it gives 0 if
-- table is empty.
lastFileId' :: SqlConn -- ^ DB connection.
            -> IO Int  -- ^ Last id.
lastFileId' c = lastFileId c
                >>= \i -> return $! fromMaybe 0 i

fileExists :: SqlConn -- ^ DB connection.
           -> File -- ^ File.
           -> IO Bool
fileExists c f =
    let st = DB.st c ! SqlFileCashe FileExists
        (d', f') = splitFileName (F.path f)
        param = [ toSql d', toSql f' ]
        conv r = (fromSql $ r ! "COUNT(f.id)" :: Int) > 0
    in execute st param >> fetchRowMap st
       >>= \e -> return $! maybe False conv e

-- | Generic single field updater.
updateField :: forall a. (Convertible a SqlValue)
            => Statement -- ^ Update statement on column to update
            -> Int       -- ^ Id of file to update
            -> a         -- ^ New column data.
            -> IO ()
updateField st i n =
    let param = [ toSql n , toSql i ]
    in void $! execute st param

-- | Update name of file.
updateName :: SqlConn -- ^ DB connection.
           -> Int     -- ^ File id.
           -> Text    -- ^ New name.
           -> IO ()
updateName (SqlConnT _ fm) = updateField (fm ! SqlFileCashe UpdateName)

-- | Update location of file.
updateLocation :: SqlConn  -- ^ DB connection.
               -> Int      -- ^ File id.
               -> FilePath -- ^ New location.
               -> IO ()
updateLocation (SqlConnT _ fm) = updateField (fm ! SqlFileCashe UpdateLocation)

-- | Update size of file.
updateSize :: SqlConn -- ^ DB connection.
           -> Int     -- ^ File id.
           -> Int64   -- ^ New size.
           -> IO ()
updateSize (SqlConnT _ fm) = updateField (fm ! SqlFileCashe UpdateSize)

-- | Update access time of tile.
updateAccessTime :: SqlConn -- ^ DB connection.
                 -> Int     -- ^ File id.
                 -> UTCTime -- ^ New access time.
                 -> IO ()
updateAccessTime (SqlConnT _ fm) = updateField (fm ! SqlFileCashe UpdateAccessTime)

-- | Update modification time of file
updateModTime :: SqlConn -- ^ DB connection.
              -> Int     -- ^ File id.
              -> UTCTime -- ^ New modification time.
              -> IO ()
updateModTime (SqlConnT _ fm) = updateField (fm ! SqlFileCashe UpdateModTime)

-- | Update user permissions of file.
updateUser :: SqlConn      -- ^ DB connection.
           -> Int          -- ^ File id.
           -> P.Permission -- ^ New user permission.
           -> IO ()
updateUser (SqlConnT _ fm) = updateField (fm ! SqlFileCashe UpdateUser)

-- | Update group permissions of file.
updateGroup :: SqlConn      -- ^ DB connection.
            -> Int          -- ^ File id.
            -> P.Permission -- ^ New group permission.
            -> IO ()
updateGroup (SqlConnT _ fm) = updateField (fm ! SqlFileCashe UpdateGroup)

-- | Update other permissions of file.
updateOther :: SqlConn      -- ^ DB connection.
            -> Int          -- ^ File id.
            -> P.Permission -- ^ New other permission.
            -> IO ()
updateOther (SqlConnT _ fm) = updateField (fm ! SqlFileCashe UpdateOther)

-- | Insert a new hash for a file.
insertHash :: SqlConn -- ^ DB connection.
           -> Int     -- ^ File id.
           -> Text    -- ^ Hash.
           -> IO ()
insertHash (SqlConnT _ fm) i h =
    let st = fm ! SqlFileCashe InsertHash
        param = [ toSql h, toSql i ]
    in void $! execute st param

-- | Remove hash of a file.
removeHash :: SqlConn -- ^ DB connection.
           -> Int     -- ^ Id of file.
           -> IO ()
removeHash (SqlConnT _ fm) i =
    let st = fm ! SqlFileCashe RemoveHash
        param = [ toSql i ]
    in void $! execute st param

-- | Update hash of a file.
updateHash :: SqlConn -- ^ DB connection.
           -> Int     -- ^ File id.
           -> Text    -- ^ Hash as text.
           -> IO ()
updateHash (SqlConnT _ fm) = updateField (fm ! SqlFileCashe UpdateHash)

-- | Update hash of a file. Hash is given as a Word128 but is converted to a
-- text.
updateHash' :: SqlConn -- ^ DB connection.
            -> Int -- ^ Id of file.
            -> Word128 -- ^ Hash.
            -> IO ()
updateHash' c i w = updateHash c i (pack $ showHex w "")

-- | Create a conduit source of all unhashed values.
getUnhashed :: MonadIO m
            => SqlConn -- ^ DB connection.
            -> C.Source m File
getUnhashed (SqlConnT _ f) =
    let toFile :: Map String SqlValue
               -> File
        toFile m = let file = fromSql $ m ! "name"
                       dir  = fromSql $ m ! "location"
                   in File { id         = fromSql $ m ! "id"
                           , path       = dir </> file
                           , size       = fromSql $ m ! "size"
                           , accessTime = fromSql $ m ! "access_time"
                           , modTime    = fromSql $ m ! "mod_time"
                           , user       = fromSql $ m ! "user_per"
                           , group      = fromSql $ m ! "group_per"
                           , other      = fromSql $ m ! "other_per"
                           }

        st = f ! SqlFileCashe GetUnhashed

        source :: MonadIO m
               => C.Source m File
        source =  liftIO (fetchRowMap st)
                  >>= maybe (return ())
                            (\r -> do C.yield (toFile r)
                                      source)

    in do liftIO $! void (execute st [])
          source

