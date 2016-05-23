{-# LANGUAGE OverloadedStrings                #-}
{-# LANGUAGE DeriveDataTypeable, TypeFamilies #-}
module Main where

import Prelude hiding (id)
import FileSystem
import Types.File (path, File)
--import Database (fileIndex)
--import qualified Database.FileCache as DB
--import Database.Persist.Sqlite
--import qualified Database.Esqueleto as E

import Control.Lens
import Control.Monad (liftM, void, when)
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

import           Graphics.QML
import           Data.Text (Text)
import qualified Data.Text as T

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
    v <- newEmptyMVar :: IO (MVar ())
    void $! installHandler keyboardSignal
                           (Catch $! handClose v)
                           Nothing

    clazz <- newClass
        [ defMethod' "scanDirectory" (\_ txt -> do
            files <- liftM (map (view path)) . getFiles $ T.unpack txt
            return . T.pack . unlines $ files :: IO Text)
        , defMethod' "cleanPath" (\_ txt ->
            let p = T.take 7 txt
                r = if p == "file://"
                        then T.drop 7 txt
                        else txt
            in return r :: IO Text)
        ]
    ctx <- newObject clazz ()

    runEngineLoop defaultEngineConfig {
        initialDocument = fileDocument "qml/test.qml",
        contextObject = Just $ anyObjRef ctx
    }

    --loop files v

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
