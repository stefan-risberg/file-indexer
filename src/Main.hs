{-# LANGUAGE OverloadedStrings          #-}
module Main where

import FileSystem
import Types.File (path)
import Database (fileIndex)
import qualified Database.FileCache as FC
import Database.Persist
import Database.Persist.Sqlite

import Control.Lens
import Control.Monad (liftM, filterM)
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = runSqlite "./test/database.test.sql" $ do
    runMigration fileIndex
    files <- (liftIO $! getAllFiles "./test/files")
             >>= filterM (liftM not  . FC.fileExists)

    liftIO $! print (length files)
    mapM_ FC.insertFile files

    FC.getUnhashed >>= liftIO . print . length

    return ()
