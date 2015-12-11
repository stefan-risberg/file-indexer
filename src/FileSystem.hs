module FileSystem
( getFileStatus
, directoryContent
, filterFiles
, filterDirectorys
, getAllFiles
) where

import           Types.File            (File (..))
import qualified Types.File       as F
import qualified Types.Permission as P

import           System.FilePath       (( </> ))
import           System.Directory      (getDirectoryContents
                                       ,canonicalizePath
                                       )
import qualified System.Posix.Files as P
import           System.Posix.Files    (isDirectory
                                       ,FileStatus
                                       ,accessTimeHiRes
                                       ,modificationTimeHiRes
                                       ,fileSize
                                       ,isRegularFile
                                       )
import           System.IO.Error       (isDoesNotExistError
                                       ,catchIOError
                                       )

import           Control.Monad         (liftM)

import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Maybe            (fromJust)

getFileStatus :: FilePath
              -> IO (Maybe FileStatus)
getFileStatus fp = catchIOError (P.getFileStatus fp >>= return . Just)
                                (\e -> if isDoesNotExistError e
                                           then return Nothing
                                           else ioError e)

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

    is <- liftM (maybe False isDirectory) (getFileStatus fp)
    if is
        then (do cont <- liftM (map (fp </>) . filter filterDots)
                               (getDirectoryContents fp)
                 stat <- mapM (liftM fromJust . getFileStatus) cont
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
    in do content <- canonicalizePath fp
                     >>= directoryContent
          let (f, d) = split content
          get' f d

