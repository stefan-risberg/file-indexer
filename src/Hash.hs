{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Hash where

import           Data.Conduit ( Sink
                              , Source
                              , await
                              , yield
                              , ($$))

import           Data.LargeWord (Word128)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Binary (decode)
import           Data.ByteString.Lazy (fromStrict)

import           Crypto.Hash.MD5 ( init
                                 , update
                                 , finalize
                                 , Ctx)

import           Control.Lens hiding (at)
import           Control.Monad.Reader
import           Control.Monad.Except

import           System.IO ( openFile
                           , IOMode (ReadMode)
                           , hClose
                           , hSeek
                           , SeekMode (AbsoluteSeek)
                           , Handle
                           )
import           Prelude hiding (init)

import           Job

data HashJob
    = HashJob
        { _idL :: Int -- ^ Id of file.
        , _fileL :: FilePath -- ^ File to hash.
        , _fileAtL :: Int -- ^ How much of the file have been consumed.
        , _ctxL :: Ctx -- ^ Hash context.
        , _finL :: Ctx -> ByteString -- ^ Finalization of hash.
        , _updateL :: Ctx -> ByteString -> Ctx -- ^ Update context.
        }

makeLenses ''HashJob

-- | Create a new HashJob.
newHashJob :: Int -- ^ File id.
           -> FilePath -- ^ File to hash.
           -> HashJob
newHashJob i f = HashJob { _idL = i
                         , _fileL = f
                         , _fileAtL = 0
                         , _ctxL = init
                         , _finL = finalize
                         , _updateL = update
                         }

-- | Reads a file with 4096 bytestrings.
readBlock :: FilePath -- ^ File to start reading.
          -> Maybe Int -- ^ Where to start reading from.
          -> Source (Job HashJob) ByteString
readBlock fp at = do
    hand <- lift . ioJob $! openFile fp ReadMode

    lift . ioJob $ maybe (return ())
                         (hSeek hand AbsoluteSeek . fromIntegral)
                         at

    run hand
    lift . ioJob $ hClose hand
    return ()
    where
        -- | Reads a block. Returns Nothing when the handle is eof.
        consumeHand :: Handle -> IO (Maybe ByteString)
        consumeHand h = do
            b <- B.hGet h 4096
            return $ if B.null b
                then Nothing
                else Just b

        run :: Handle
            -> Source (Job HashJob) ByteString
        run h = do
            lift (ioJob $ consumeHand h)
            >>= maybe (return ())
                      (\bs -> yield bs >> run h)

-- | Consumes bytestring source and creates a hash. Can be stopped.
hashSink :: HashJob -- ^ Hash state.
         -> Sink ByteString (Job HashJob) (Either HashJob Word128)
hashSink j = do
    --halt <- liftIO p
    lift (pauseJob j)
    await >>= maybe end hash

    where
        hash bs = let len = B.length bs
                      ctx = (j ^. updateL) (j ^. ctxL) bs
                      j' = j & fileAtL .~ (len + (j ^. fileAtL))
                             & ctxL .~ ctx
                  in hashSink j'
        end = return $! Right (decode . fromStrict . (j ^. finL) $ j ^. ctxL)

-- | Runs a HashJob.
runHashJob :: HashJob
           -> IO (JobKey HashJob Word128)
runHashJob j =
    runJob job

    where
        job :: Job HashJob Word128
        job = do
            r <- readBlock (j ^. fileL) (Just $ j ^. fileAtL)
              $$ hashSink j

            case r of
                Left x -> Job $ throwError x
                Right x -> return x
