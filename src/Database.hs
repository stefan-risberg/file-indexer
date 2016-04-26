{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database where

import Database.Persist.TH
import Data.Text (Text)
import Data.Word (Word8, Word64)
import Data.Time.Clock (UTCTime)
import Data.ByteString (ByteString)

share [mkPersist sqlSettings { mpsGenerateLenses = True }, mkMigrate "fileIndex"] [persistLowerCase|
File
    name       Text
    location   Text
    size       Word64
    accessTime UTCTime
    modTime    UTCTime
    userPer    Word8
    groupPer   Word8
    otherPer   Word8
    UniquePath location name
    deriving   Show

Job
    file       FileId
    ctx        ByteString
    consumed   Int
    deriving   Show

Hash
    file     FileId
    hash     Text
    deriving Show
|]
