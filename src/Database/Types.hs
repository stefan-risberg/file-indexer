{-# LANGUAGE OverloadedStrings          #-}

module Database.Types
( SqlFunc (..)
, FuncMap
, SqlConn (..)
) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Map.Strict
import qualified Database.State.Types as State
import qualified Database.FileCache.Types as FileCache

data SqlFunc = SqlState State.Function
             | SqlFileCashe FileCache.Function
             deriving (Eq, Ord)

type FuncMap = Map SqlFunc Statement

data SqlConn = SqlConn Connection FuncMap
