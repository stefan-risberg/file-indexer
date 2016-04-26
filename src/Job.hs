{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Job
( JobKey (..)
, Job (..)
, pauseL
, retL
, runJob
, pauseJob
, getPause
, ioJob
, jobDone
, jobPause
, jobResult
) where

import Control.Concurrent.MVar
import Control.Lens
import Control.Concurrent (forkIO)
import Control.Monad.Reader
import Control.Monad.Except


type JobPause = MVar ()
data JobKey e a
    = JobKey
        { _pauseL :: !JobPause
        , _retL :: !(MVar (Either e a))
        }
makeLenses ''JobKey

newJobKey :: IO (JobKey e a)
newJobKey = do
    p <- newEmptyMVar
    r <- newEmptyMVar

    return $! JobKey p r

newtype Job e a = Job { runJob' :: ExceptT e (ReaderT JobPause IO) a }
                deriving (Functor, Applicative, Monad)

runJob :: Job e a
       -> IO (JobKey e a)
runJob j = do
    key <- newJobKey

    _ <- forkIO (do
         g <- runReaderT (runExceptT (runJob' j)) (key ^. pauseL)
         putMVar (key ^. retL) g)

    return key

-- | If the job should be paused, pause it and return intermidiet result.
pauseJob :: e -- ^ Intermidiet job result.
         -> Job e ()
pauseJob e = Job (do
    p <- ask >>= liftM not . liftIO . isEmptyMVar

    if p
        then throwError e
        else return ()
    )

ioJob :: IO a -> Job e a
ioJob i = Job (do
    i' <- liftIO i
    return i')


getPause :: Job e JobPause
getPause = Job (ask)

jobDone :: JobKey e a
        -> IO Bool
jobDone = liftM not . isEmptyMVar . view pauseL

jobPause :: JobKey e a
         -> IO (Either e a)
jobPause k = putMVar (k ^. pauseL) () >> takeMVar (k ^. retL)

jobResult :: JobKey e a
          -> IO a
jobResult k = do
    takeMVar (k ^. retL)
    >>= either (\_ -> fail "Something when realy wrong...")
               return
