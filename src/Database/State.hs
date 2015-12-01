{-# LANGUAGE FlexibleContexts #-}
module Database.State
( sql
, create
, insert
, lastState
) where

import Database.State.Types
import Database.Types
import qualified Database.Common as C
import Database.HDBC
import Database.HDBC.Sqlite3 (Connection)
import Data.Text (Text)
import Data.Map.Strict ((!))
import Control.Monad (void)

sql :: [(Function, String)]
sql = [(Insert, "INSERT INTO state_history (id, state) \n\
                \VALUES (?, ?);")
      ,(LastState, "SELECT state FROM state_history \n\
                   \WHERE id = (SELECT MAX(id) FROM state_history);")
      ,(MaxId, "SELECT MAX(id) FROM state_history;")
      ]

create :: Connection
       -> IO ()
create c = void $
    run c "CREATE TABLE IF NOT EXISTS state_history \n\
          \     ( id INTEGER \n\
          \     , state TEXT NOT NULL \n\
          \     , PRIMARY KEY (id) \n\
          \     );" []

insert :: Text
       -> SqlConn
       -> IO Int
insert t (SqlConn _ f) =
    let ins     = f ! SqlState Insert
        m       = f ! SqlState MaxId
        param i = [toSql i, toSql t]
    in C.nextId m >>= \id_ -> execute ins (param id_) >> return id_

lastState :: SqlConn
          -> IO (Maybe Text)
lastState (SqlConn _ f) =
    let l = f ! SqlState LastState
    in void (execute l []) >> C.readHead l
