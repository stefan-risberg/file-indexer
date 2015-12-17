module Database.HashStatus
( sql
, create

, FileId
, Offset
, Ctx
, Done

, new
, update
, done
, get
) where

import           Database.HashStatus.Types

import           Database.Types hiding (conn, st)
import qualified Database.Types as DB

import           Database.HDBC.Sqlite3 (Connection)
import           Database.HDBC

import           Data.ByteString hiding (concat)
import           Data.Map.Strict ((!), Map)
import           Data.Conduit (Source, yield)

import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Lens

type FileId = Integer
type Offset = Integer
type Ctx = ByteString
type Done = Bool

-- | All hash status sql queries.
sql :: [(Function, String)] -- ^ Sql queris with their enums.
sql = [ ( New
        , unwords $
            "INSERT INTO hash_status (file, at, ctx)":
            "VALUES (?, ?, ?);": [])
      , ( UpdateProgress
        , unwords $
            "UPDATE hash_status":
            "   SET at = ?":
            " WHERE file = ?;": [])
      , ( UpdateCtx
        , unwords $
            "UPDATE hash_status":
            "   SET ctx = ?":
            " WHERE file = ?;": [])
      , ( Clean
        , unwords $
            "DELETE FROM hash_status":
            " WHERE done = 1;": [])
      , ( Done
        , unwords $
            "UPDATE hash_status":
            "   SET done = 1":
            " WHERE file = ?;": [])
      , ( Get
        , unwords $
            "SELECT file, at, ctx":
            "  FROM hash_status":
            " WHERE done = 0;": [])
      ]

-- | Create the hash status table.
create :: Connection -- ^ Sql connection.
       -> IO ()
create c = do
    void $ run c (concat $ "CREATE TABLE IF NOT EXISTS hash_status":
                           "   ( file INTEGER":
                           "   , at INTEGER":
                           "   , ctx BLOB":
                           "   , done INTEGER DEFAULT 0":
                           "   , PRIMARY KEY (file)":
                           "   , FOREIGN KEY (file) REFERENCES(id);": []) []

-- | Add new hash status.
new :: SqlConn               -- ^ DB connection.
    -> (FileId, Offset, Ctx) -- ^ Data to insert.
    -> IO ()
new c (file, off, ctx) =
    let st    = (view DB.st c) ! SqlHashStatus New
        param = [ toSql file
                , toSql off
                , toSql ctx
                ]
     in void $! execute st param

-- | Update hash status.
update :: SqlConn               -- ^ DB connection.
       -> (FileId, Offset, Ctx) -- ^ Data to update.
       -> IO ()
update c (file, off, ctx) =
    let uOff  = m ! SqlHashStatus UpdateProgress
        uCtx  = m ! SqlHashStatus UpdateCtx
        m = c ^. DB.st
        f = [ toSql file ]
    in void $! execute uOff (toSql off : f)
            >> execute uCtx (toSql ctx : f)

-- | Mark hash status as done.
done :: SqlConn -- ^ DB connection.
     -> FileId  -- ^ Hash to mark as done.
     -> IO ()
done c i =
    let st = (c ^. DB.st) ! SqlHashStatus Done
        param = [ toSql i ]
    in void $! execute st param

-- | A source of all not done file hashes.
get :: MonadIO m
    => SqlConn
    -> Source m (FileId, Offset, Ctx)
get c =
    let st = (c ^. DB.st) ! SqlHashStatus Get
        conv :: Map String SqlValue
             -> (FileId, Offset, Ctx)
        conv m = ( fromSql $ m ! "file"
                 , fromSql $ m ! "at"
                 , fromSql $ m ! "ctx"
                 )
        get' :: MonadIO m
             => Source m (FileId, Offset, Ctx)
        get' = do
            liftIO $ fetchRowMap st
            >>= maybe (return ())
                      (\m -> yield (conv m) >> get')
    in do liftIO $ void $ execute st []
          get'
