module Database.Job
( newJob
, updateJob
, completeJob
, getJob
) where

import Database
import qualified Database.Esqueleto as E
import Database.Esqueleto ( (=.)
                          , (==.)
                          , val
                          )

import Control.Lens
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (void)

-- | Add a new job.
newJob :: MonadIO m
       => Job -- ^ Job to add.
       -> E.SqlPersistT m ()
newJob j = void $! E.insert j

-- | Update the progress of job.
updateJob :: MonadIO m
          => Job
          -> E.SqlPersistT m ()
updateJob j =
    E.update $ \job -> do
        E.set job [ JobAt =. (val $ j ^. jobAt)
                  , JobCtx =. (val $ j ^. jobCtx)
                  ]
        E.where_ $ job E.^. JobFile ==. (val $ j ^. jobFile)

-- | Complete job.
completeJob :: MonadIO m
            => Job
            -> E.SqlPersistT m ()
completeJob j =
    E.update $ \job -> do
        E.set job [ JobAt =. (val 0)
                  , JobCtx =. (val $! j ^. jobCtx)
                  , JobDone =. (val True)
                  ]
        E.where_ $ job E.^. JobFile ==. (val $ j ^. jobFile)

-- | Get a single job, if Nothing no jobs are pending.
getJob :: MonadIO m
       => E.SqlPersistT m (Maybe Job)
getJob = do
    j <- E.select $
         E.from $ \job -> do
             E.limit 1
             E.where_ (job E.^. JobDone ==. (val False))
             return job
    if null j
        then return Nothing
        else return $! Just (E.entityVal . head $ j)
