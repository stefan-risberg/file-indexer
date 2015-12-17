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
import Database.Types hiding (conn, st)
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
import Control.Lens

import Numeric
import Types.File
import Prelude hiding (id)
import qualified Types.File as F
import qualified Types.Permission as P

import System.FilePath (splitFileName, (</>))

import qualified Data.Conduit as C
import Data.Int (Int64)

sql :: [(Function, String)]
sql = [ ( InsertFile
        , unwords $ "INSERT INTO files (id, name, location, size, access_time, mod_time, user_per, group_per, other_per)":
                    "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?);": [])
      , ( RemoveFile
        , unwords $ " DELETE FROM files":
                    "  WHERE id=?;":
                    " DELETE FROM hashes":
                    "  WHERE file_id=?;": [])
      , ( LastFileId
        , unwords $ "SELECT MAX(id) FROM files;":[])

      , ( FileExists
        , unwords $ "SELECT COUNT(f.id)":
                    "  FROM files AS f":
                    " WHERE f.location = ?":
                    "   AND f.name = ?;":[])

      , ( UpdateName
        , unwords $ "UPDATE files":
                    "   SET name=?":
                    " WHERE id=?;":[])
      , ( UpdateLocation
        , unwords $ "UPDATE files":
                    "   SET location=?":
                    " WHERE id=?;":[])
      , ( UpdateSize
        , unwords $ "UPDATE files":
                    "   SET size=?":
                    " WHERE id=?;":[])
      , ( UpdateAccessTime
        , unwords $ "UPDATE files":
                    "   SET access_time=?":
                    " WHERE id=?;":[])
      , ( UpdateModTime
        , unwords $ "UPDATE files":
                    "   SET mod_time=?":
                    " WHERE id=?;":[])
      , ( UpdateUser
        , unwords $ "UPDATE files":
                    "   SET user_per=?":
                    " WHERE id=?;":[])
      , ( UpdateGroup
        , unwords $ "UPDATE files":
                    "   SET group_per=?":
                    " WHERE id=?;":[])
      , ( UpdateOther
        , unwords $ "UPDATE files":
                    "   SET other_per=?":
                    " WHERE id=?;":[])

      , ( InsertHash
        , unwords $ "INSERT INTO hashes (file_id, hash:[])":
                    "SELECT f.id, ?":
                    "  FROM files AS f":
                    " WHERE f.id=?;":[])
      , ( RemoveHash
        , unwords $ "DELETE FROM hashes":
                    " WHERE file_id=?;":[])
      , ( UpdateHash
        , unwords $ "UPDATE hashes":
                    "   SET hash=?":
                    " WHERE file_id=?;":[])
      , ( GetUnhashed
        , unwords $ "SELECT f.id":
                    "      ,f.name":
                    "      ,f.location":
                    "      ,f.size":
                    "      ,f.access_time":
                    "      ,f.mod_time":
                    "      ,f.user_per":
                    "      ,f.group_per":
                    "      ,f.other_per":
                    "  FROM files AS f":
                    " WHERE f.id NOT IN (SELECT h.file_id":
                    "                    FROM hashes AS h);":[])
      ]

create :: Connection
       -> IO ()
create c = do
    void $ run c
         (concat $ "CREATE TABLE IF NOT EXISTS files":
                   " ( id INTEGER":
                   " , name TEXT NOT NULL":
                   " , location TEXT NOT NULL":
                   " , size TEXT NOT NULL":
                   " , access_time TEXT NOT NULL":
                   " , mod_time TEXT NOT NULL":
                   " , user_per INTEGER NOT NULL":
                   " , group_per INTEGER NOT NULL":
                   " , other_per INTEGER NOT NULL":
                   " , PRIMARY KEY (id)":
                   " );":[]) []
    void $ run c
         (concat $ "CREATE TABLE IF NOT EXISTS hashes":
                   "     ( file_id INTEGER":
                   "     , hash TEXT NOT NULL":
                   "     , PRIMARY KEY (file_id)":
                   "     , FOREIGN KEY (file_id) REFERENCES files(id)":
                   "     );": []) []

-- | Add a file that has an id already.
insertFile :: SqlConn -- ^ DB connection.
           -> File -- ^ File to insert.
           -> IO ()
insertFile c file =
    let ins = (c ^. DB.st) ! SqlFileCashe InsertFile
        (d, f) = splitFileName (view F.path file)
        param     = [ toSql $ view F.id file
                    , toSql f
                    , toSql d
                    , toSql $ view F.size file
                    , toSql $ view F.accessTime file
                    , toSql $ view F.modTime file
                    , toSql $ view F.user file
                    , toSql $ view F.group file
                    , toSql $ view F.other file
                    ]
    in void $! execute ins param

-- | Add new file.
insertFile' :: SqlConn -- ^ DB connection.
            -> File    -- ^ File to insert.
            -> IO File -- ^ File with update id field.
insertFile' c file =
    liftM (+1) (lastFileId' c)
    >>= \i -> insertFile c (set F.id i file) >> (return $! set F.id i file)

-- | Remove file.
removeFile :: SqlConn -- ^ DB connection.
           -> Int     -- ^ Id of file to remove.
           -> IO ()
removeFile c i =
    let st = (c ^. DB.st)  ! SqlFileCashe RemoveFile
        sI = toSql i
        param = [ sI, sI ]
    in void $! execute st param

-- | Get last file id. If Nothing the file table is empty.
lastFileId :: SqlConn        -- ^ DB connection.
           -> IO (Maybe Int) -- ^ Last id.
lastFileId c =
    let st = (c ^. DB.st)  ! SqlFileCashe LastFileId
        conv r = let r' = r ! "MAX(id)"
                 in case r' of
                        SqlNull -> Nothing
                        a -> Just $! fromSql a
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
    let st = (c ^. DB.st)  ! SqlFileCashe FileExists
        (d', f') = splitFileName (view F.path f)
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
                   in File { _id         = fromSql $ m ! "id"
                           , _path       = dir </> file
                           , _size       = fromSql $ m ! "size"
                           , _accessTime = fromSql $ m ! "access_time"
                           , _modTime    = fromSql $ m ! "mod_time"
                           , _user       = fromSql $ m ! "user_per"
                           , _group      = fromSql $ m ! "group_per"
                           , _other      = fromSql $ m ! "other_per"
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

