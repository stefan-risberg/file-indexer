module Database
( connect
, disconnect
, commit
) where

import Database.HDBC hiding (disconnect, commit)
import qualified Database.HDBC as DB
import Database.HDBC.Sqlite3
import qualified Database.State as State
import qualified Database.FileCache as FileCache
import Database.Types

import Data.Map.Strict hiding (map, foldr)

import Control.Monad (liftM)

-- | Connect to database
connect :: FilePath   -- ^ Location of database
        -> IO SqlConn -- ^ Connection.
connect loc = do c <- connectSqlite3 loc
                 mapM_ (\f -> f c) [ State.create
                                   , FileCache.create
                                   ]
                 stateFunc <- connect' c SqlState State.sql
                 fileCacheFunc <- connect' c SqlFileCashe FileCache.sql

                 return $ SqlConnT c
                        $ fromList
                        $ concat [ stateFunc
                                 , fileCacheFunc
                                 ]
              where
                connect' :: Connection
                         -> (a -> SqlFunc)
                         -> [(a, String)]
                         -> IO [(SqlFunc, Statement)]
                connect' c t =
                    let prep = prepare c
                        m (f, q) = liftM (\st' -> (t f, st')) (prep q)
                    in mapM m

-- | Disconnect from the database.
disconnect :: SqlConn -- ^ DB connection.
           -> IO ()
disconnect c = DB.disconnect $ conn c

-- | Commit changes to the database.
commit :: SqlConn -- ^ DB connection.
       -> IO ()
commit c = DB.commit $ conn c
