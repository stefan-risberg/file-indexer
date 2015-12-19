{-# LANGUAGE OverloadedStrings          #-}
module Main where

import FileSystem
import Types.File (path)
import Database
import qualified Database.FileCache as FC

import Control.Lens
import Control.Monad (liftM, filterM)

import System.Environment (getArgs)

main :: IO ()
main = do
    c <- connect "./test/dasabase.test.sql"
    files <- getAllFiles "./test/files"
             >>= filterM (\f -> FC.fileExists c f >>= return . not)
    mapM_ (\x -> print (x ^. path)) files

    return ()
