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

import           Data.Word (Word64)
import           Data.Int (Int64)
import qualified Types.Permission as P
import           Data.Time.Clock

import           Control.Lens

import           Prelude hiding (id)

data File = File { _id         :: Maybe Int64
                 , _path       :: FilePath
                 , _size       :: Word64
                 , _accessTime :: UTCTime
                 , _modTime    :: UTCTime
                 , _user       :: P.Permission
                 , _group      :: P.Permission
                 , _other      :: P.Permission
                 }

makeLenses ''File
