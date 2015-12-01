{-# LANGUAGE OverloadedStrings          #-}

module Storage
where

import Database.HDBC
import Database.HDBC.Sqlite3

import Control.Monad (void, liftM)
import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import Data.Text (Text)

data State = Init
           | Hashing
           deriving (Eq)

TH.share [TH.mkPersist TH.sqlSettings, TH.mkMigrate "database"] [TH.persistLowerCase|
FileCache
    location FilePath
    hashed Bool
    hash Text
    deriving Show
StateHistory
    state Text
    deriving Show
|]

--initTables :: Connection
--           -> IO ()
--initTables c = do
--    fileCacheStmt <- prepare c $ "create table if not exists file_cache(" ++
--                                 "id integer not null, " ++
--                                 "filepath text not null, " ++
--                                 "hashed integer default 0, " ++
--                                 "hash text, " ++
--                                 "primary key (id));"
--
--    stateStmt <- prepare c $ "create table if not exists state_history(" ++
--                             "id integer not null, " ++
--                             "state text not null, " ++
--                             "primary key (id));"
--
--    mapM_ (\x -> execute x []) [ fileCacheStmt
--                               , stateStmt
--                               ]
--
--    return ()
--
--newState :: State -> Connection -> IO ()
--newState s c | s == Init = newState' "init"
--             | s == Hashing = newState' "hashing"
--             | otherwise = return ()
--    where
--        newState' :: String -> IO ()
--        newState' t = do st <- prepare c $ "insert into state_history (state)" ++
--                                           "values ({});"
--                         void $ execute st [toSql t]
--
--getState :: Connection -> IO (Maybe State)
--getState c = do st <- prepare c $ "select state from state_history " ++
--                                  " where id = (select max(id) from state_history);"
--                void $ execute st []
--                r <- fetchRow st >>= \x -> liftM (fromMaybe (toSql "") (head))
--                case r of
--                    Just ["init"] -> return $! Just Init
--                    Just ["hashing"] -> return $! Just Hashing
--                    _ -> return Nothing
--
--initialize :: FilePath
--           -> IO Connection
--initialize fp = do
--    c <- connectSqlite3 fp
--
--    initTables c
--
--    return c
