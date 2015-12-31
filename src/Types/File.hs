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
, conv
) where

import           Data.Word (Word64)
import           Data.Int (Int64)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import           Control.Lens

import           System.Posix.Files    (accessTimeHiRes
                                       ,modificationTimeHiRes
                                       ,fileSize
                                       ,isRegularFile
                                       )
import           System.Posix.Files (FileStatus)

import qualified Types.Permission as P

import           Prelude hiding (id)

data File =
    File { _id         :: !(Maybe Int64)
         , _path       :: !FilePath
         , _size       :: !Word64
         , _accessTime :: !UTCTime
         , _modTime    :: !UTCTime
         , _user       :: !P.Permission
         , _group      :: !P.Permission
         , _other      :: !P.Permission
         }

makeLenses ''File

-- | Convert a filestatus witha filepath to a File.
conv :: FilePath -- ^ Location of file.
     -> FileStatus -- ^ Status of file.
     -> Maybe File
conv fp fs =
    let aT = posixSecondsToUTCTime . accessTimeHiRes
        mT = posixSecondsToUTCTime . modificationTimeHiRes
        c = File { _id = Nothing
                 , _path = fp
                 , _size = fromIntegral (fileSize fs)
                 , _accessTime = aT fs
                 , _modTime = mT fs
                 , _user = P.Permission True True False
                 , _group = P.Permission True True False
                 , _other = P.Permission True False False
                 }
    in if isRegularFile fs
        then Just c
        else Nothing
