{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
module GHC.Event.Windows (
    -- * Manager
    Manager,
    new,
    getSystemManager,
    postIO,

    -- * Overlapped I/O
    associateHandle,
    withOverlapped,
    StartCallback,
    CompletionCallback,
    Overlapped(..),

    -- * Timeouts
    TimeoutCallback,
    TimeoutKey,
    Seconds,
    registerTimeout,
    updateTimeout,
    unregisterTimeout,
) where

import GHC.Event.Windows.Clock   (Clock, Seconds, getClock, getTime)
import GHC.Event.Windows.FFI     (Overlapped(..))
import qualified GHC.Event.Windows.FFI    as FFI
import qualified GHC.Event.PSQ            as Q

import Control.Exception as E
import Control.Concurrent
import Data.IORef
import Data.Maybe
import Data.Tuple
import Data.Word
import GHC.Base
import GHC.Event.Unique
import GHC.Num
import GHC.Real
import GHC.Windows
import System.IO.Unsafe     (unsafePerformIO)

------------------------------------------------------------------------
-- Manager

data Manager = Manager
    { mgrIOCP         :: !(FFI.IOCP ManagerCallback)
    , mgrClock        :: !Clock
    , mgrUniqueSource :: !UniqueSource
    }

type ManagerCallback = ErrCode -> DWORD -> Mgr ()

new :: IO Manager
new = do
    mgrIOCP         <- FFI.newIOCP
    mgrClock        <- getClock
    mgrUniqueSource <- newSource
    let mgr = Manager{..}
    _tid <- forkIO $ loop mgr
    return mgr

getSystemManager :: IO (Maybe Manager)
getSystemManager = readIORef managerRef

managerRef :: IORef (Maybe Manager)
managerRef = unsafePerformIO $
    if rtsSupportsBoundThreads
        then new >>= newIORef . Just
        else newIORef Nothing
{-# NOINLINE managerRef #-}

newOverlapped :: Word64 -> ManagerCallback -> IO Overlapped
newOverlapped = FFI.newOverlapped

-- | Queue an action to be performed by the I/O manager thread.
postIO :: Manager -> IO () -> IO ()
postIO mgr = postMgr mgr . liftIO

-- | Variant of 'postIO' that allows the callback to modify the
-- timeout queue.
postMgr :: Manager -> Mgr () -> IO ()
postMgr mgr cb = newOverlapped 0 (\_errCode _numBytes -> cb)
             >>= FFI.postCompletion (mgrIOCP mgr) 0

------------------------------------------------------------------------
-- Overlapped I/O

-- | Callback that starts the overlapped I/O operation.
-- It must return successfully if and only if an I/O completion has been
-- queued.  Otherwise, it must throw an exception, which 'withOverlapped'
-- will rethrow.
type StartCallback = Overlapped -> IO ()

-- | Called when the completion is delivered.
type CompletionCallback a = ErrCode   -- ^ 0 indicates success
                         -> DWORD     -- ^ Number of bytes transferred
                         -> IO a

-- | Associate a 'HANDLE' with the I/O manager's completion port.  This must be
-- done before using the handle with 'withOverlapped'.
associateHandle :: Manager -> HANDLE -> IO ()
associateHandle Manager{..} h =
    FFI.associateHandleWithIOCP mgrIOCP h

-- | Start an overlapped I/O operation, and wait for its completion.  If
-- 'withOverlapped' is interrupted by an asynchronous exception, the operation
-- will be canceled using @CancelIo@.
--
-- 'withOverlapped' waits for a completion to arrive before returning or
-- throwing an exception.  This means you can use functions like
-- 'Foreign.Marshal.Alloc.alloca' to allocate buffers for the operation.
withOverlapped :: HANDLE
               -> Word64 -- ^ Value to use for the @OVERLAPPED@
                         --   structure's Offset/OffsetHigh members.
               -> StartCallback
               -> CompletionCallback a
               -> IO a
withOverlapped h offset startCB completionCB = do
    signal <- newEmptyMVar
    let signalReturn a = do _ <- tryPutMVar signal $ return a
                            return ()
        signalThrow ex = do _ <- tryPutMVar signal $ throwIO
                                 (ex :: SomeException)
                            return ()
    mask_ $ do
        let completionCB' e b = liftIO $
                (completionCB e b >>= signalReturn) `E.catch` signalThrow
        ol <- newOverlapped offset completionCB'

        startCB ol `E.catch` \ex -> do FFI.discardOverlapped ol
                                       signalThrow ex

        let cancel = uninterruptibleMask_ $ FFI.cancelIoEx h ol
        join (takeMVar signal `onException` cancel)

------------------------------------------------------------------------
-- Timeouts

type TimeoutQueue = Q.PSQ TimeoutCallback

-- |
-- Warning: since the 'TimeoutCallback' is called from the I/O manager, it must
-- not throw an exception or block for a long period of time.  In particular,
-- be wary of 'Control.Exception.throwTo' and 'Control.Concurrent.killThread':
-- if the target thread is making a foreign call, these functions will block
-- until the call completes.
type TimeoutCallback = IO ()

newtype TimeoutKey = TK Unique
    deriving (Eq, Ord)

-- | Register an action to be performed in the given number of seconds.  The
-- returned 'TimeoutKey' can be used to later unregister or update the timeout.
-- The timeout is automatically unregistered when it fires.
--
-- The 'TimeoutCallback' will not be called more than once.
registerTimeout :: Manager -> Seconds -> TimeoutCallback -> IO TimeoutKey
registerTimeout mgr@Manager{..} relTime cb = do
    key <- newUnique mgrUniqueSource
    now <- getTime mgrClock
    let !expTime = now + relTime
    postMgr mgr $ modifyTQ $ Q.insert key expTime cb
    return $ TK key

-- | Update an active timeout to fire in the given number of seconds (from the
-- time 'updateTimeout' is called), instead of when it was going to fire.
-- This has no effect if the timeout has already fired.
updateTimeout :: Manager -> TimeoutKey -> Seconds -> IO ()
updateTimeout mgr (TK key) relTime = do
    now <- getTime (mgrClock mgr)
    let !expTime = now + relTime
    postMgr mgr $ modifyTQ $ Q.adjust (const expTime) key

-- | Unregister an active timeout.  This is a harmless no-op if the timeout is
-- already unregistered or has already fired.
--
-- Warning: the timeout callback may fire even after
-- 'unregisterTimeout' completes.
unregisterTimeout :: Manager -> TimeoutKey -> IO ()
unregisterTimeout mgr (TK key) =
    postMgr mgr $ modifyTQ $ Q.delete key

------------------------------------------------------------------------
-- The Mgr state monad

newtype Mgr a = Mgr { runMgr :: TimeoutQueue -> IO (a, TimeoutQueue) }

instance Functor Mgr where
    fmap = liftM

instance Applicative Mgr where
    pure a = Mgr $ \s -> return (a, s)
    (<*>)  = ap

instance Monad Mgr where
    return = pure
    m >>= k = Mgr $ \s -> do
        (a, s') <- runMgr m s
        runMgr (k a) s'

liftIO :: IO a -> Mgr a
liftIO io = Mgr $ \s -> do
    a <- io
    return (a, s)

getsTQ :: (TimeoutQueue -> a) -> Mgr a
getsTQ f = Mgr $ \s -> return (f s, s)

modifyTQ :: (TimeoutQueue -> TimeoutQueue) -> Mgr ()
modifyTQ f = Mgr $ \s -> do
    let !s' = f s
    return ((), s')

stateTQ :: (TimeoutQueue -> (a, TimeoutQueue)) -> Mgr a
stateTQ f = Mgr $ \s -> do
    let (a, !s') = f s
    return (a, s')

mapM_        :: Monad m => (a -> m b) -> [a] -> m ()
{-# INLINE mapM_ #-}
mapM_ f xs    =  foldr (>>) (return ()) (map f xs)

------------------------------------------------------------------------
-- I/O manager loop

-- | Call all expired timeouts, and return how much time until the next
-- | expiration.
runExpiredTimeouts :: Manager -> Mgr (Maybe Seconds)
runExpiredTimeouts Manager{..} = do
    -- Avoid calling getTime when there are no pending expirations.
    isNull <- getsTQ Q.null
    if isNull then
        return Nothing
    else do
        now <- liftIO $ getTime mgrClock

        -- Remove timeouts with expiration <= now, and execute their callbacks.
        expired <- stateTQ $ Q.atMost now
        mapM_ (liftIO . Q.value) expired

        -- See how soon the next timeout expires.
        next <- getsTQ $ fmap Q.prio . Q.findMin
        case next of
            Nothing ->
                return Nothing
            Just t -> do
                -- This value will always be positive since the call
                -- to 'atMost' above removed any timeouts <= 'now'
                let !t' = t - now
                return $ Just t'

-- | Return the delay argument to pass to GetQueuedCompletionStatus.
fromTimeout :: Maybe Seconds -> Word32
fromTimeout Nothing                 = 120000
fromTimeout (Just sec) | sec > 120  = 120000
                       | sec > 0    = ceiling (sec * 1000)
                       | otherwise  = 0

step :: Manager -> Mgr ()
step mgr@Manager{..} = do
    delay <- runExpiredTimeouts mgr
    m <- liftIO $ FFI.getNextCompletion mgrIOCP (fromTimeout delay)
    case m of
        Nothing                      -> return ()
        Just (cb, numBytes, errCode) -> cb errCode numBytes

loop :: Manager -> IO loop
loop mgr = go Q.empty
  where go s = runMgr (step mgr) s >>= go . snd
