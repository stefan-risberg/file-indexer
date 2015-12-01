{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Database.Common
( maxId
, nextId
, readHead
) where

import Database.HDBC
import Data.Maybe (fromMaybe)
import Control.Monad (liftM, void)
import Data.Convertible.Base

maxId :: Statement
      -> IO (Maybe Int)
maxId st = do void (execute st [])
              fetchRow st >>= \x -> case x of
                Just [i] -> return (fromSql i)
                _        -> return Nothing

nextId :: Statement
       -> IO Int
nextId = liftM((+) 1 . fromMaybe 0) . maxId

readHead :: Convertible SqlValue a
         => Statement
         -> IO (Maybe a)
readHead st = fetchRow st >>= \x -> case x of
    Just (i:_) -> return (Just $ fromSql i)
    _          -> return Nothing
