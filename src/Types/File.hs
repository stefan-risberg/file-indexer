{-# LANGUAGE TemplateHaskell #-}

module Types.File
( File (..)
, id
, path
, size
, accessTime
, modTime
, user
, group
, other
) where

import           Data.Int (Int64)
import qualified Types.Permission as P
import           Data.Time.Clock

import           Control.Lens

import           Prelude hiding (id)

data File = File { _id         :: Int
                 , _path       :: FilePath
                 , _size       :: Int64
                 , _accessTime :: UTCTime
                 , _modTime    :: UTCTime
                 , _user       :: P.Permission
                 , _group      :: P.Permission
                 , _other      :: P.Permission
                 }

makeLenses ''File
