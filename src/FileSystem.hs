module FileSystem
( fileStatus
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

fileStatus :: FilePath
           -> IO (Maybe FileStatus)
fileStatus fp = catchIOError (P.getFileStatus fp >>= return . Just)
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

    is <- liftM (maybe False isDirectory) (fileStatus fp)
    if is
        then (do cont <- liftM (map (fp </>) . filter filterDots)
                               (getDirectoryContents fp)
                 stat <- mapM (liftM fromJust . fileStatus) cont
                 return $! zip cont stat)
        else return []

-- |Get all files.
filterFiles :: [(FilePath, FileStatus)] -- ^ List to filter.
            -> [File] -- ^ List of all files.
filterFiles = let aT = posixSecondsToUTCTime . accessTimeHiRes
                  mT = posixSecondsToUTCTime . modificationTimeHiRes
              in map (\(fp, fs) -> File { F._id = Nothing
                                        , F._path = fp
                                        , F._size = fromIntegral (fileSize fs)
                                        , F._accessTime = aT fs
                                        , F._modTime = mT fs
                                        , F._user = P.Permission True True False
                                        , F._group = P.Permission True True False
                                        , F._other = P.Permission True False False
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

