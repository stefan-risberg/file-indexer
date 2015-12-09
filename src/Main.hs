{-# LANGUAGE OverloadedStrings          #-}
module Main where

--import System.IO (stdout, hFlush)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)

import Data.Conduit

import Data.LargeWord (Word128)
import Data.Binary (decode)

import Control.Monad.Trans.Resource
import Control.Concurrent.MVar

import qualified Crypto.Hash.MD5 as C

import qualified Database as DB
import qualified Database.State as DB.State
import qualified Database.FileCache as DB.FileCache

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

main :: IO ()
main = do c <- DB.connect "fisk.sql"

          i <- DB.FileCache.lastFileId' c

          print i

          DB.commit c
          return ()

