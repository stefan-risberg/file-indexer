{-# LANGUAGE OverloadedStrings          #-}
module Main where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)

import Data.Conduit

import Data.LargeWord (Word128)
import Data.Binary (decode)

import System.FilePath ((</>))

import Control.Monad (liftM
                     ,unless
                     )
import Control.Monad.Trans.Resource
import Control.Concurrent.MVar

import qualified Crypto.Hash.MD5 as C

import qualified Database as DB
import Database.Types (SqlConn)
--import qualified Database.State as DB.State
import qualified Database.FileCache as DB.FileCache

import qualified FileSystem as FS

-- |Hash sink.
hashConduit :: MonadResource m
            => Conduit ByteString -- ^ To hash
                       m
                       Word128    -- ^ Hash of bytestring
hashConduit = h C.init
    where
        h ctx = do
            d <- await
            case d of
                Nothing -> yield $! decode (fromStrict (C.finalize ctx))
                Just bs -> h $! C.update ctx bs


closeSignal :: MVar () -> IO ()
closeSignal v = putMVar v ()

testDatabase :: IO SqlConn
testDatabase = do
    let testLocation = "./test"
        testDB       = testLocation </> "database.test.sql"
        testFiles    = testLocation </> "files"

    ex <- liftM (maybe False (\_ -> True)) (FS.getFileStatus testDB)

    c <- DB.connect $ testDB

    unless ex (FS.getAllFiles testFiles
                   >>= mapM_ (DB.FileCache.insertFile' c)
                   >>  DB.commit c)
    return c

main :: IO ()
main = do
    c <- testDatabase

    DB.disconnect c

    return ()

