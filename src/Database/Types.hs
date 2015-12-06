{-# LANGUAGE OverloadedStrings          #-}

module Database.Types
( SqlFunc (..)
, FuncMap
, SqlConnT (..)
, SqlConn
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

data SqlConnT a b = SqlConnT
    { conn :: a
    , st :: b
    }

type SqlConn = SqlConnT Connection FuncMap
