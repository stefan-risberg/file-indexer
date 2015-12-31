{-# LANGUAGE TemplateHaskell #-}

module FileSystem
( getFilePaths
, getFiles
, fileSource
) where

import           System.FilePath       (( </> ))
import           System.Directory      (getDirectoryContents
                                       ,canonicalizePath
                                       )

import           System.Posix.Files    (isRegularFile
                                       ,isDirectory
                                       ,getSymbolicLinkStatus
                                       )

import           Control.Monad (liftM)
import           Control.Monad.IO.Class

import           Data.Conduit
import           Data.Conduit.List (consume)
import           Data.Maybe (fromJust)

import           Types.File (File, conv)

io :: MonadIO m
   => IO a
   -> m a
io = liftIO

filterDots :: [FilePath]
           -> [FilePath]
filterDots = filter (\a -> a `notElem` [".", ".."])

getFilePaths :: FilePath
             -> IO [FilePath]
getFilePaths fp = fileSource fp $$ consume

getFiles :: FilePath
         -> IO [File]
getFiles fp =
    let fpToFile :: MonadIO m => Conduit FilePath m File
        fpToFile = await
                   >>= maybe (return ())
                             (\f -> do fs <- io (getSymbolicLinkStatus f)
                                       yield (fromJust $ conv f fs)
                                       fpToFile)
    in fileSource fp $= fpToFile $$ consume

fileSource :: MonadIO m
           => FilePath
           -> Source m FilePath
fileSource fp =
    let cont :: MonadIO m => [FilePath] -> Source m FilePath
        cont [] = return ()
        cont (f:fs) =
            let handFile = yield f >> cont fs
                handDir = liftM filterDots (io . getDirectoryContents $ f)
                          >>= return . map (f </>)
                          >>= \c -> cont (fs ++ c)
                hand s | isRegularFile s = handFile
                       | isDirectory s   = handDir
                       | otherwise       = cont fs
            in io (getSymbolicLinkStatus f) >>= hand
    in io (canonicalizePath fp) >>= \f -> cont [f]
