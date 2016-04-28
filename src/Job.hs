{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Job
( JobKey (..)
, Job (..)
, pauseL
, retL
, spawnJob
, pauseJob
, getPause
, jobDone
, jobPause
, jobResult
) where

import           Control.Concurrent.MVar
import           Control.Lens
import           Control.Concurrent (forkIO)
import           Control.Monad.Reader
import           Control.Monad.Except

type JobPause = MVar ()

-- | Key to pause and get result from async jobs.
data JobKey e a
    = JobKey
        { _pauseL :: !JobPause          -- ^ Pause key
        , _retL :: !(MVar (Either e a)) -- ^ Return value.
        }
makeLenses ''JobKey

-- | Create a new job key.
newJobKey :: IO (JobKey e a)
newJobKey = do
    p <- newEmptyMVar
    r <- newEmptyMVar

    return $! JobKey p r

-- | Job monad.
newtype Job e a = Job { runJob :: ExceptT e (ReaderT JobPause IO) a }
                deriving (Functor, Applicative, Monad, MonadIO)

-- | Spawns a job. Use the JobKey to controll execution.
spawnJob :: Job e a
         -> IO (JobKey e a)
spawnJob j = do
    key <- newJobKey

    _ <- forkIO (do
         g <- runReaderT (runExceptT (runJob j)) (key ^. pauseL)
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

-- | Get the pause operation inside the job monad.
getPause :: Job e JobPause
getPause = Job (ask)

-- | Check if the job has finished.
jobDone :: JobKey e a
        -> IO Bool
jobDone = liftM not . isEmptyMVar . view retL

-- | Pause the job and return the finished result or intermidiet result.
jobPause :: JobKey e a -- ^ JobKey associated with the job to cancel.
         -> IO (Either e a) -- ^ Result.
jobPause k = putMVar (k ^. pauseL) () >> takeMVar (k ^. retL)

-- | Wait for the complete result of the job.
jobResult :: JobKey e a -- ^ JobKey associated with the job to wait for.
          -> IO a
jobResult k = do
    takeMVar (k ^. retL)
    >>= either (\_ -> fail "Something when realy wrong...")
               return
