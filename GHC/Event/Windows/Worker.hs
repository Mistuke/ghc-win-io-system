{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
module GHC.Event.Windows.Worker (
    Worker,
    new,
    enqueue,

    -- * Helpers
    forkOSUnmasked,
) where

import Data.IORef
import GHC.Base
import GHC.Conc.Sync
import GHC.IO
import GHC.MVar
import Control.Concurrent

data Worker = Worker
    { workerJobs :: !(IORef JobList)
    , workerWake :: !(MVar ())
    }

instance Eq Worker where
    Worker a _ == Worker b _ = a == b

-- | @'forever' act@ repeats the action infinitely.
forever     :: (Monad m) => m a -> m b
{-# INLINE forever #-}
forever a   = let a' = a >> a' in a'

-- | Fork an OS thread, and return a handle for sending jobs to it.
new :: IO Worker
new = do
    workerJobs <- newIORef id
    workerWake <- newEmptyMVar
    _ <- forkOSUnmasked $ forever $ do
        takeMVar workerWake
        jobs <- atomicModifyIORef workerJobs $ \jobs -> (id, jobs)
        runJobList jobs
    return Worker{..}

-- | Add a job to the work queue.  Jobs are executed in the order they are
-- queued, and every job is run in the same OS thread.
--
-- A job should not block for long or throw an exception, as this will prevent
-- future jobs from running.
--
-- Exception safety:  atomic, non-interruptible
enqueue :: Worker -> IO () -> IO ()
enqueue Worker{..} io =
    mask_ $ do
        !() <- atomicModifyIORef workerJobs $ \jobs -> (snocJobList jobs io, ())
        _ <- tryPutMVar workerWake ()
        return ()

------------------------------------------------------------------------
-- Helpers

forkOSUnmasked :: IO () -> IO ThreadId
forkOSUnmasked = forkOS . unsafeUnmask

-- A difference list, but with (x >>) instead of (x :)
type JobList = IO () -> IO ()

-- | Append an action to the job list, so it will
-- run /after/ the existing actions.
snocJobList :: JobList -> IO () -> JobList
snocJobList dl io = dl . (io >>)

runJobList :: JobList -> IO ()
runJobList dl = dl (return ())
