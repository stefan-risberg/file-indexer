{-# LANGUAGE OverloadedStrings          #-}
module Main where

import Prelude hiding (id)
import FileSystem
import Data.Maybe (fromJust)
import Types.File (path, File, id)
import Database (fileIndex)
import qualified Database.FileCache as DB
import Database.Persist.Sqlite
import qualified Database.Esqueleto as E

import Control.Lens
import Control.Monad (liftM, filterM, void)
import Control.Monad.IO.Class (liftIO, MonadIO)

import Control.Concurrent.MVar
import Control.Concurrent (threadDelay)

import System.Posix.Signals
import System.FilePath (takeFileName)
import System.Environment (getArgs)

import Hash (hash)

io :: MonadIO m
   => IO a
   -> m a
io = liftIO

handClose :: MVar ()
          -> IO ()
handClose v = do
    putStrLn $! "Close signal"
    putMVar v ()

runHashes :: MonadIO m
          => MVar () -- ^ To check if Ctrl-C was pressed
          -> [File] -- ^ Files to hash.
          -> E.SqlPersistT m ()
runHashes _ [] = return ()
runHashes v (f:fs) = do
    io $! putStrLn $ "Hashing -- " ++ (takeFileName $! f ^. path)
    (io $! hash (f ^. path))
        >>= DB.insertHash' (fromJust $! f ^. id)
    io $! threadDelay 0
    e <- io $ isEmptyMVar v
    if e
        then runHashes v fs
        else return ()

main :: IO ()
main = do
    dir <- liftM head getArgs

    v <- newEmptyMVar :: IO (MVar ())
    void $! installHandler keyboardSignal
                           (Catch $! handClose v)
                           Nothing

    runSqlite "file-cache.db" $ do
        runMigration fileIndex
        (io $! getAllFiles dir)
            >>= filterM (liftM not  . DB.fileExists)
            >>= mapM_ DB.insertFile

        io $ putStrLn "Start file hasing:"
        DB.getUnhashed >>= runHashes v
        io $ putStrLn "Done hashing and result is written to database."

        return ()
    return ()
