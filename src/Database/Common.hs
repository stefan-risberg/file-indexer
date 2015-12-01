{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes #-}

module Database.Common
( maxId
, nextId
, readHead
, updateFields
, updateField
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

-- | Generic function to update fields by the row id.
updateFields :: Statement -- ^ DB statement.
             -> Int -- ^ Row id.
             -> [SqlValue]
             -> IO ()
updateFields st i = void . execute st . (:) (toSql i)

-- | Update a single filed by its id.
updateField :: forall a. (Convertible a SqlValue)
            => Statement -- ^ DB statement.
            -> Int -- ^ Row id.
            -> a -- ^ data.
            -> IO ()
updateField st i d = updateFields st i [toSql d]
