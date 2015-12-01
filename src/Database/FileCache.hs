{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Database.FileCache
( sql
, create

, insertFile
, removeFile
, lastFileId
, lastFileId'

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
import Database.Types
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
                 \ , CONSTRAINT permissions \n\
                 \        CHECK (user_per >= 0 AND user_per < 8 \n\
                 \          AND group_per >= 0 AND group_per < 8 \n\
                 \          AND other_per >= 0 AND other_per < 8) \n\
                 \ , CONSTRAINT unique_file \n\
                 \        UNIQUE (location,name) \n\
                 \ , PRIMARY KEY (id)\n\
                 \ );" []
    void $ run c "CREATE TABLE IF NOT EXISTS hashes \n\
                 \     ( file_id INTEGER \n\
                 \     , hash TEXT NOT NULL \n\
                 \     , PRIMARY KEY (file_id) \n\
                 \     , FOREIGN KEY (file_id) REFERENCES files(id) \n\
                 \     );" []

-- | Add new file.
insertFile :: SqlConn -- ^ DB connection.
           -> File    -- ^ File to insert.
           -> IO File -- ^ File with update id field.
insertFile c@(SqlConn _ fm) file = do
    i <- liftM (+1) (lastFileId' c)
    let ins       = fm ! SqlFileCashe InsertFile
        (d, f)    = splitFileName (F.path file)
        param     = [ toSql i
                    , toSql f
                    , toSql d
                    , toSql $ F.size file
                    , toSql $ F.accessTime file
                    , toSql $ F.modTime file
                    , toSql $ F.user file
                    , toSql $ F.group file
                    , toSql $ F.other file
                    ]

    void $ execute ins param
    return file { id = i }

-- | Remove file.
removeFile :: SqlConn -- ^ DB connection.
           -> Int     -- ^ Id of file to remove.
           -> IO ()
removeFile (SqlConn _ fm) i =
    let st = fm ! SqlFileCashe RemoveFile
        sI = toSql i
        param = [ sI, sI ]
    in void $! execute st param

-- | Get last file id. If Nothing the file table is empty.
lastFileId :: SqlConn        -- ^ DB connection.
           -> IO (Maybe Int) -- ^ Last id.
lastFileId (SqlConn _ fm) =
    let st = fm ! SqlFileCashe LastFileId
        conv r = Just $ fromSql (r ! "id")
    in liftM (maybe Nothing conv)
             (execute st [] >> fetchRowMap st)

-- | Get last file id. Same as 'lastFileId' except it gives 0 if
-- table is empty.
lastFileId' :: SqlConn -- ^ DB connection.
            -> IO Int  -- ^ Last id.
lastFileId' = liftM (fromMaybe 0) . lastFileId

-- | Generic single field updater.
updateField :: forall a. (Convertible a SqlValue)
            => Statement -- ^ Update statement on column to update
            -> Int       -- ^ Id of file to update
            -> a         -- ^ New column data.
            -> IO ()
updateField st i n =
    let param = [ toSql n , toSql i ]
    in void $ execute st param

-- | Update name of file.
updateName :: SqlConn -- ^ DB connection.
           -> Int     -- ^ File id.
           -> Text    -- ^ New name.
           -> IO ()
updateName (SqlConn _ fm) = updateField (fm ! SqlFileCashe UpdateName)

-- | Update location of file.
updateLocation :: SqlConn  -- ^ DB connection.
               -> Int      -- ^ File id.
               -> FilePath -- ^ New location.
               -> IO ()
updateLocation (SqlConn _ fm) = updateField (fm ! SqlFileCashe UpdateLocation)

-- | Update size of file.
updateSize :: SqlConn -- ^ DB connection.
           -> Int     -- ^ File id.
           -> Int64   -- ^ New size.
           -> IO ()
updateSize (SqlConn _ fm) = updateField (fm ! SqlFileCashe UpdateSize)

-- | Update access time of tile.
updateAccessTime :: SqlConn -- ^ DB connection.
                 -> Int     -- ^ File id.
                 -> UTCTime -- ^ New access time.
                 -> IO ()
updateAccessTime (SqlConn _ fm) = updateField (fm ! SqlFileCashe UpdateAccessTime)

-- | Update modification time of file
updateModTime :: SqlConn -- ^ DB connection.
              -> Int     -- ^ File id.
              -> UTCTime -- ^ New modification time.
              -> IO ()
updateModTime (SqlConn _ fm) = updateField (fm ! SqlFileCashe UpdateModTime)

-- | Update user permissions of file.
updateUser :: SqlConn      -- ^ DB connection.
           -> Int          -- ^ File id.
           -> P.Permission -- ^ New user permission.
           -> IO ()
updateUser (SqlConn _ fm) = updateField (fm ! SqlFileCashe UpdateUser)

-- | Update group permissions of file.
updateGroup :: SqlConn      -- ^ DB connection.
            -> Int          -- ^ File id.
            -> P.Permission -- ^ New group permission.
            -> IO ()
updateGroup (SqlConn _ fm) = updateField (fm ! SqlFileCashe UpdateGroup)

-- | Update other permissions of file.
updateOther :: SqlConn      -- ^ DB connection.
            -> Int          -- ^ File id.
            -> P.Permission -- ^ New other permission.
            -> IO ()
updateOther (SqlConn _ fm) = updateField (fm ! SqlFileCashe UpdateOther)

-- | Insert a new hash for a file.
insertHash :: SqlConn -- ^ DB connection.
           -> Int     -- ^ File id.
           -> Text    -- ^ Hash.
           -> IO ()
insertHash (SqlConn _ fm) i h =
    let st = fm ! SqlFileCashe InsertHash
        param = [ toSql h, toSql i ]
    in void $ execute st param

-- | Remove hash of a file.
removeHash :: SqlConn -- ^ DB connection.
           -> Int     -- ^ Id of file.
           -> IO ()
removeHash (SqlConn _ fm) i =
    let st = fm ! SqlFileCashe RemoveHash
        param = [ toSql i ]
    in void $ execute st param

-- | Update hash of a file.
updateHash :: SqlConn -- ^ DB connection.
           -> Int     -- ^ File id.
           -> Text    -- ^ Hash as text.
           -> IO ()
updateHash (SqlConn _ fm) = updateField (fm ! SqlFileCashe UpdateHash)

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
getUnhashed (SqlConn _ f) =
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

    in do liftIO $ void (execute st [])
          source

