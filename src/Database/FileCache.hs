module Database.FileCache
( insertFile
, removeFile

, fileExists

, updateName
, updateLocation
, updateSize
, updateAccessTime
, updateModTime
, updateUser
, updateGroup
, updateOther

, insertHash
, insertHash'
, removeHash

, updateHash
, updateHash'
, getUnhashed
) where

import Database
import Database.Esqueleto

import Data.Text (pack, unpack, Text)
import Data.Int (Int64)
import Data.Word (Word64)
import Data.Time (UTCTime)
import Data.LargeWord (Word128)

import qualified Types.File as F
import Types.Permission (permissionToWord, Permission, wordToPermission)

import System.FilePath (takeFileName, dropFileName, (</>))

import qualified Control.Lens as L
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (void)

import Numeric (showHex)

-- | Add a file. If id is Nothing it is set to new given id.
--   If it is Just i the id on the database will be set to i.
insertFile :: MonadIO m
           => F.File                      -- ^ File to add
           -> SqlPersistT m F.File -- ^ File returned with the id set.
insertFile file =
    let f = File (pack $! takeFileName $ L.view F.path file)
                 (pack $! dropFileName $ L.view F.path file)
                 (L.view F.size file)
                 (L.view F.accessTime file)
                 (L.view F.modTime file)
                 (permissionToWord $ L.view F.user file)
                 (permissionToWord $ L.view F.group file)
                 (permissionToWord $ L.view F.other file)

    in maybe (insert f >>= \k -> return $! L.set F.id (Just (fromSqlKey k)) file)
             (\i -> insertKey (toSqlKey i)
                              f
                        >> return file)
             (L.view F.id file)

-- | Removes a file.
removeFile :: MonadIO m
           => F.File -- ^ File to remove.
           -> SqlPersistT m ()
removeFile file = maybe (return ())
                        (\i -> delete $
                               from $ \f ->
                               where_ (f ^. FileId ==. val (toSqlKey i :: Key File)))
                        (L.view F.id file)

-- | Check for file existance.
fileExists :: MonadIO m
           => F.File -- ^ File to check if it exists.
           -> SqlPersistT m Bool
fileExists file =
    select (from $ \n -> do
                let a = count (n ^. FileId)
                    name = pack . takeFileName $ L.view F.path file
                    loc = pack . dropFileName $ L.view F.path file

                where_ (n ^. FileName ==. val name &&. n ^. FileLocation ==. val loc)
                return a)
         >>= \c -> return $! 0 < ((unValue $ head c) :: Int)

-- | Generic update of field for a file.
updateField :: (MonadIO m, PersistField typ)
            => EntityField File typ
            -> Int64
            -> typ
            -> SqlPersistT m ()
updateField f i d =
    update $ \n -> do
        set n [ f =. val d ]
        where_ (n ^. FileId ==. val (toSqlKey i))


-- | Update name of file.
updateName :: MonadIO m
           => Int64    -- ^ File id
           -> FilePath -- ^ New Name
           -> SqlPersistT m ()
updateName i n = updateField FileName i (pack n)

-- | Update location of file.
updateLocation :: MonadIO m
               => Int64    -- ^ File id.
               -> FilePath -- ^ New location.
               -> SqlPersistT m ()
updateLocation i fp = updateField FileLocation i (pack fp)

-- | Update size of file.
updateSize :: MonadIO m
           => Int64  -- ^ File id.
           -> Word64 -- ^ New size.
           -> SqlPersistT m ()
updateSize = updateField FileSize

-- | Update access time of tile.
updateAccessTime :: MonadIO m
                 => Int64   -- ^ File id.
                 -> UTCTime -- ^ New access time.
                 -> SqlPersistT m ()
updateAccessTime = updateField FileAccessTime

-- | Update modification time of file
updateModTime :: MonadIO m
              => Int64     -- ^ File id.
              -> UTCTime -- ^ New modification time.
              -> SqlPersistT m ()
updateModTime = updateField FileModTime

-- | Update user permissions of file.
updateUser :: MonadIO m
           => Int64      -- ^ File id.
           -> Permission -- ^ New user permission.
           -> SqlPersistT m ()
updateUser i p = updateField FileUserPer i (permissionToWord p)

-- | Update group permissions of file.
updateGroup :: MonadIO m
            => Int64        -- ^ File id.
            -> Permission -- ^ New group permission.
            -> SqlPersistT m ()
updateGroup i p = updateField FileGroupPer i (permissionToWord p)

-- | Update other permissions of file.
updateOther :: MonadIO m
            => Int64      -- ^ File id.
            -> Permission -- ^ New other permission.
            -> SqlPersistT m ()
updateOther i p = updateField FileOtherPer i (permissionToWord p)

-- | Insert a new hash for a file.
insertHash :: MonadIO m
           => Int64   -- ^ File id.
           -> Text    -- ^ Hash.
           -> SqlPersistT m ()
insertHash i h = void $ insert $ Hash (toSqlKey i) h

-- | Insert a new hash for a file.
insertHash' :: MonadIO m
            => Int64   -- ^ File id.
            -> Word128 -- ^ Hash.
            -> SqlPersistT m ()
insertHash' i h = insertHash i (pack $ showHex h "")

-- | Remove hash of a file.
removeHash :: MonadIO m
           => Int64 -- ^ Id of file.
           -> SqlPersistT m ()
removeHash i = delete
             $ from
             $ \h -> where_ (h ^. HashFile ==. val (toSqlKey i))

-- | Update hash of a file.
updateHash :: MonadIO m
           => Int64 -- ^ File id.
           -> Text  -- ^ Hash as text.
           -> SqlPersistT m ()
updateHash i t =
    update $ \h -> do
        set h [ HashHash =. val t ]
        where_ (h ^. HashFile ==. val (toSqlKey i))

-- | Update hash of a file.
updateHash' :: MonadIO m
            => Int64
            -> Word128
            -> SqlPersistT m ()
updateHash' i w =
    updateHash i (pack $ showHex w "")

-- | Create a conduit source of all unhashed values.
getUnhashed :: MonadIO m
            => SqlPersistT m [F.File]
getUnhashed =
    let conv :: Int64 -> File -> F.File
        conv i f = F.File { F._id = Just i
                          , F._path = (unpack $ f L.^. fileLocation)
                                  </> (unpack $ f L.^. fileName)
                          , F._size = f L.^. fileSize
                          , F._accessTime = f L.^. fileAccessTime
                          , F._modTime = f L.^. fileModTime
                          , F._user = wordToPermission $! f L.^. fileUserPer
                          , F._group = wordToPermission $! f L.^. fileGroupPer
                          , F._other = wordToPermission $! f L.^. fileOtherPer
                          }
    in (select $
        from $ \file -> do
            where_ $ notExists $
                     from $ \hash -> do
                     where_ (hash ^. HashFile ==. file ^. FileId)
            return file)
           >>= mapM (\f -> let v = entityVal f
                               i = fromSqlKey (entityKey f)
                            in return $! conv i v)
