{-# LANGUAGE OverloadedStrings          #-}
module Main where

import Prelude hiding (id)
import FileSystem
--import Data.Maybe (fromJust)
import Types.File (path, File)
--import Database (fileIndex)
--import qualified Database.FileCache as DB
--import Database.Persist.Sqlite
--import qualified Database.Esqueleto as E

import Control.Lens
import Control.Monad (liftM, void)
import Control.Monad.IO.Class (liftIO, MonadIO)

import Control.Concurrent.MVar
import Control.Concurrent (threadDelay)

import System.Posix.Signals
--import System.FilePath (takeFileName)
import System.Environment (getArgs)

import qualified Hash as H
import qualified Job as J

import           Data.LargeWord (Word128)
import           Numeric (showHex)

io :: MonadIO m
   => IO a
   -> m a
io = liftIO

handClose :: MVar ()
          -> IO ()
handClose v = do
    putStrLn $! "Close signal"
    putMVar v ()

getHex :: Word128 -> String
getHex w = let h = showHex w ""
               zero = map (const '0') [0 .. (31 - (length h))]
            in zero ++ h

main :: IO ()
main = do
    dir <- liftM head getArgs

    v <- newEmptyMVar :: IO (MVar ())
    void $! installHandler keyboardSignal
                           (Catch $! handClose v)
                           Nothing

    files <- getFiles dir

    loop files v

    return ()

    where
        hashLoop :: J.JobKey H.HashJob Word128 -> MVar () -> IO (Maybe Word128)
        hashLoop key pause = do
            threadDelay 50
            p <- liftM not (isEmptyMVar pause)
            k <- J.jobDone key

            if p
                then (J.jobPause key >> return Nothing)
                else if k
                    then J.jobResult key >>= return . Just
                    else hashLoop key pause
        loop :: [File] -> MVar () -> IO ()
        loop [] _ = putStrLn "End of all files."
        loop (f:fx) p = do
            let hj = H.newHashJob 0 (f ^. path)
            key <- H.runHashJob hj
            r <- hashLoop key p

            case r of
                Just x -> (do putStrLn $! (getHex x) ++ ":  " ++ (f ^. path)
                              loop fx p)
                Nothing -> (putStrLn "Interrupted")
