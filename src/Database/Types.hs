{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Database.Types
( SqlFunc (..)
, FuncMap
, SqlConnT (..)
, SqlConn
, conn
, st
) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Map.Strict
import qualified Database.State.Types as State
import qualified Database.FileCache.Types as FileCache
import qualified Database.HashStatus.Types as HashStatus
import           Control.Lens

data SqlFunc = SqlState State.Function
             | SqlFileCashe FileCache.Function
             | SqlHashStatus HashStatus.Function
             deriving (Eq, Ord, Show)

type FuncMap = Map SqlFunc Statement

data SqlConnT a b = SqlConnT
    { _conn :: a
    , _st :: b
    }

makeLenses ''SqlConnT

type SqlConn = SqlConnT Connection FuncMap
