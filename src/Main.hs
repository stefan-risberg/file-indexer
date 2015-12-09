{-# LANGUAGE OverloadedStrings          #-}
module Main where

--import System.IO (stdout, hFlush)
import System.FilePath ((</>))
import System.Environment (getArgs)
import System.Directory (getDirectoryContents)
import System.Posix.Signals (Handler (CatchOnce)
                            ,installHandler
                            ,keyboardSignal
                            )
import System.Posix.Files (getFileStatus
                          ,fileSize
                          ,isRegularFile
                          ,isDirectory
                          ,FileStatus
                          ,accessTimeHiRes
                          ,modificationTimeHiRes
                          )

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)

import Data.Conduit
import Data.Conduit.Combinators (sourceFile)

import Data.LargeWord (Word128)
import Data.Binary (decode)

import qualified Data.List as L

import Control.Monad
--import Control.Monad.IO.Class
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

          DB.commit c
          return ()

--main :: IO ()
--main = do
--       folder <- liftM head getArgs
--       files  <- getAllFiles folder
--       sig <- newEmptyMVar
--
--       let proc :: [Content] -- ^ To scan
--                -> Int64     -- ^ Size scanned
--                -> IO ()
--           proc [] _ = printf "\nDone with scanning.\n"
--           proc (f:fx) s = do printf "%c[2K\r" (27 :: Int)
--                              printf "Scanned: %s of %s."
--                                     (toSize $ Byte s)
--                                     (toSize $ Byte totalSize)
--                              hFlush stdout
--                              hashFile (contPath f)
--                              proc fx (s + contSize f)
--           dups = getDuplicateSizes files
--           totalSize = sum (map contSize dups)
--
--       _ <- installHandler keyboardSignal
--                                    (CatchOnce (closeSignal sig))
--                                    Nothing
--       printf "We have %s to scan.\n" (show $ length dups)
--
--       --proc (getDuplicateSizes files) 0
--       liftIO $ putStrLn "hasning done."
