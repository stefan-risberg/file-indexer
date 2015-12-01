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
import Data.Conduit.List (sinkNull, consume)
import Data.Conduit.Combinators (sourceFile)

import Data.LargeWord (Word128)
import Data.Binary (decode)

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
--import Data.Maybe (fromMaybe)

--import Data.Int (Int64)
import qualified Data.List as L
--import Data.Text (Text, empty)

import Control.Monad
--import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Concurrent.MVar

import qualified Crypto.Hash.MD5 as C

import qualified Database as DB
import qualified Database.State as DB.State
import qualified Database.FileCache as DB.FileCache

--import Numeric

import Text.Printf

import Types
import Types.File (File (..))
import qualified Types.File as F
import qualified Types.Permission as P

-- |Strict directory and status fetcher. Returns empty if no content or not a
-- directory.
directoryContent :: FilePath
              -> IO [(FilePath, FileStatus)]
directoryContent fp = do
    let filterDots :: FilePath
                   -> Bool
        filterDots "." = False
        filterDots ".." = False
        filterDots _ = True

    is <- liftM isDirectory (getFileStatus fp)
    if is
        then (do cont <- liftM (map (fp </>) . filter filterDots)
                               (getDirectoryContents fp)
                 stat <- mapM getFileStatus cont
                 return $! zip cont stat)
        else return []

-- |Get all files.
filterFiles :: [(FilePath, FileStatus)] -- ^ List to filter.
            -> [File] -- ^ List of all files.
filterFiles = let aT = posixSecondsToUTCTime . accessTimeHiRes
                  mT = posixSecondsToUTCTime . modificationTimeHiRes
              in map (\(fp, fs) -> File { F.id = 0
                                        , F.path = fp
                                        , F.size = fromIntegral (fileSize fs)
                                        , F.accessTime = aT fs
                                        , F.modTime = mT fs
                                        , F.user = P.Permission True True False
                                        , F.group = P.Permission True True False
                                        , F.other = P.Permission True False False
                                        })
               . filter (isRegularFile . snd)

-- |Get all folders.
filterDirectorys :: [(FilePath, FileStatus)] -- ^ List to filter.
                 -> [FilePath] -- ^ List of all files.
filterDirectorys = map fst . filter (isDirectory . snd)

-- |Get all files conteind in folder and its subfolders.
getAllFiles :: FilePath
            -> IO [File]
getAllFiles fp =
    let split :: [(FilePath, FileStatus)]
              -> ([File], [FilePath])
        split x = (filterFiles x, filterDirectorys x)

        get' :: [File]
             -> [FilePath]
             -> IO [File]
        get' files [] = return files
        get' f (d:dirs) = do cont <- directoryContent d
                             let (f', d') = split cont
                             get' (f ++ f') (dirs ++ d')
    in do content <- directoryContent fp
          let (f, d) = split content
          get' f d

-- |Hash sink.
hashConduit :: MonadResource m => Conduit ByteString -- ^ To hash
                                          m
                                          Word128    -- ^ Hash of bytestring
hashConduit = h C.init
    where
        h ctx = do
            d <- await
            case d of
                Nothing -> yield $! decode (fromStrict (C.finalize ctx))
                Just bs -> h $! C.update ctx bs

getDuplicateSizes :: [File]
                  -> [File]
getDuplicateSizes = concat
                  . filter (\x -> length x > 1)
                  . L.groupBy (\a b -> F.size a == F.size b)

hashFile :: FilePath -> IO ()
hashFile fp = runResourceT $ sourceFile fp $= hashConduit $$ sinkNull

closeSignal :: MVar () -> IO ()
closeSignal v = putMVar v ()

main :: IO ()
main = do c <- DB.connect "fisk.sql"
          --filterM (liftM not . DB.FileCache.pathExists c . contPath) files
          --  >>= mapM_ (DB.FileCache.insert c . contPath)

          --f <- DB.FileCache.getUnhashed c $$ consume

          --let s = sum $ map F.size f
          --printf "File count: %d\nFile size: %s\n" (length f) (toSize (Byte s))
          --mapM_ (\(i, (fp, _, s)) -> printf "%d %s %s\n" i fp (toSize $ Byte s)) f


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
