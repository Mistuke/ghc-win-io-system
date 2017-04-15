{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module GHC.Event.Windows (
    -- * Manager
    Manager,
    getSystemManager,

    -- * Overlapped I/O
    associateHandle,
    associateHandle',
    withOverlapped,
    withOverlappedEx,
    StartCallback,
    CompletionCallback,
    LPOVERLAPPED,

    -- * Timeouts
    TimeoutCallback,
    TimeoutKey,
    Seconds,
    registerTimeout,
    updateTimeout,
    unregisterTimeout,

    -- * Utilities
    withException,
    ioSuccess,
    ioFailed,

    -- * IO Result type
    IOResult(..)
) where

import GHC.Event.Windows.Clock   (Clock, Seconds, getClock, getTime)
import GHC.Event.Windows.FFI     (OVERLAPPED, LPOVERLAPPED, OVERLAPPED_ENTRY(..))
import qualified GHC.Event.Windows.FFI    as FFI
import qualified GHC.Event.PSQ            as Q
import qualified GHC.Event.IntTable       as IT

import Control.Exception as E
import Control.Concurrent
import Data.IORef
import Data.Foldable (mapM_)
import Data.Maybe
import Data.Word
import Foreign       hiding (new)
import Foreign.ForeignPtr.Unsafe
import qualified GHC.Event.Array    as A
import GHC.Arr (Array, (!), listArray)
import GHC.Base
import GHC.List (replicate)
import GHC.Event.Unique
import GHC.Num
import GHC.Real
import GHC.Windows
import System.IO.Unsafe     (unsafePerformIO)

import qualified System.Win32.Types as Win32

------------------------------------------------------------------------
-- Manager

type IOCallback = CompletionCallback ()

data CompletionData = CompletionData {-# UNPACK #-} !(ForeignPtr OVERLAPPED)
                                     !IOCallback

data IOResult a
  = IOSuccess { ioValue :: a }
  | IOFailed  { ioErrCode :: Maybe Int }


data Manager = Manager
    { mgrIOCP         :: {-# UNPACK #-} !FFI.IOCP
    , mgrClock        ::                !Clock
    , mgrUniqueSource :: {-# UNPACK #-} !UniqueSource
    , mgrTimeouts     :: {-# UNPACK #-} !(IORef TimeoutQueue)
    , mgrCallbacks    :: {-# UNPACK #-}
                         !(Array Int (MVar (IT.IntTable CompletionData)))
    , mgrOverlappedEntries
                      :: {-#UNPACK #-} !(A.Array OVERLAPPED_ENTRY)
    }

new :: IO Manager
new = do
    mgrIOCP         <- FFI.newIOCP
    mgrClock        <- getClock
    mgrUniqueSource <- newSource
    mgrTimeouts     <- newIORef Q.empty
    mgrCallbacks    <- fmap (listArray (0, callbackArraySize-1)) $
           replicateM callbackArraySize (newMVar =<< IT.new 8)
    mgrOverlappedEntries <- A.new 64
    let !mgr = Manager{..}
    _tid <- forkIO $ loop mgr
    return mgr
      where
        replicateM n x = sequence (replicate n x)


getSystemManager :: IO (Maybe Manager)
getSystemManager = readIORef managerRef

managerRef :: IORef (Maybe Manager)
managerRef = unsafePerformIO $
    if rtsSupportsBoundThreads
        then new >>= newIORef . Just
        else newIORef Nothing
{-# NOINLINE managerRef #-}

-- must be power of 2
callbackArraySize :: Int
callbackArraySize = 32

lpoverlappedToInt :: LPOVERLAPPED -> Int
lpoverlappedToInt lpol = fromIntegral (ptrToIntPtr lpol)
{-# INLINE lpoverlappedToInt #-}

hashOverlapped :: LPOVERLAPPED -> Int
hashOverlapped lpol = (lpoverlappedToInt lpol) .&. (callbackArraySize - 1)
{-# INLINE hashOverlapped #-}

callbackTableVar :: Manager -> LPOVERLAPPED -> MVar (IT.IntTable CompletionData)
callbackTableVar mgr lpol = mgrCallbacks mgr ! hashOverlapped lpol
{-# INLINE callbackTableVar #-}

------------------------------------------------------------------------
-- Overlapped I/O

-- | Callback that starts the overlapped I/O operation.
-- It must return successfully if and only if an I/O completion has been
-- queued.  Otherwise, it must throw an exception, which 'withOverlapped'
-- will rethrow.
type StartCallback a = LPOVERLAPPED -> IO a

-- | Called when the completion is delivered.
type CompletionCallback a = ErrCode   -- ^ 0 indicates success
                          -> DWORD     -- ^ Number of bytes transferred
                          -> IO a

associateHandle' :: HANDLE -> IO ()
associateHandle' hwnd
  = do mngr <- getSystemManager
       maybe (return ()) (flip associateHandle hwnd) mngr

-- | Associate a 'HANDLE' with the I/O manager's completion port.  This must be
-- done before using the handle with 'withOverlapped'.
associateHandle :: Manager -> HANDLE -> IO ()
associateHandle Manager{..} h =
    FFI.associateHandleWithIOCP mgrIOCP h 0

-- | Start an overlapped I/O operation, and wait for its completion.  If
-- 'withOverlapped' is interrupted by an asynchronous exception, the operation
-- will be canceled using @CancelIoEx@.
--
-- 'withOverlapped' waits for a completion to arrive before returning or
-- throwing an exception.  This means you can use functions like
-- 'Foreign.Marshal.Alloc.alloca' to allocate buffers for the operation.
withOverlappedThreaded_ :: Manager
                        -> String
                        -> HANDLE
                        -> Word64 -- ^ Value to use for the @OVERLAPPED@
                                  --   structure's Offset/OffsetHigh members.
                        -> StartCallback (Maybe Int)
                        -> CompletionCallback (IOResult a)
                        -> IO (IOResult a)
withOverlappedThreaded_ mgr fname h offset startCB completionCB = do
    signal <- newEmptyMVar :: IO (MVar (IOResult a))
    let signalReturn a = tryPutMVar signal (IOSuccess a) >> return ()
        signalThrow ex = tryPutMVar signal (IOFailed ex) >> return ()
    mask_ $ do
        let completionCB' e b = completionCB e b >>= \result ->
                                  case result of
                                    IOSuccess val -> signalReturn val
                                    IOFailed  err -> signalThrow err
        fptr <- FFI.allocOverlapped offset
        let lpol = unsafeForeignPtrToPtr fptr
        _ <- withMVar (callbackTableVar mgr lpol) $ \tbl ->
             IT.insertWith (flip const) (lpoverlappedToInt lpol)
               (CompletionData fptr completionCB') tbl

        startCB lpol `onException` (Just `fmap` Win32.getLastError) >>= \result ->
          case result of
            Nothing -> return ()
            Just err -> do
                _ <- withMVar (callbackTableVar mgr lpol) $ \tbl ->
                    IT.delete (lpoverlappedToInt lpol) tbl
                signalThrow (Just err)

        let cancel = uninterruptibleMask_ $ FFI.cancelIoEx h lpol
        let runner = do res <- takeMVar signal `onException` cancel
                        case res of
                          IOFailed err -> FFI.throwWinErr fname (maybe 0 fromIntegral err)
                          _            -> return res
        runner

-- This will block the current haskell thread
-- but will allow you to cancel the operation from
-- another haskell thread since they are on the same
-- OS thread.
withOverlappedNonThreaded_ :: String
                           -> HANDLE
                           -> Word64 -- ^ Value to use for the @OVERLAPPED@
                                     --   structure's Offset/OffsetHigh members.
                           -> StartCallback (Maybe Int)
                           -> CompletionCallback (IOResult a)
                           -> IO (IOResult a)
withOverlappedNonThreaded_ _fname h offset startCB completionCB = do
    let signalReturn a = return $ IOSuccess a
        signalThrow ex = return $ IOFailed ex
    mask_ $ do
        let completionCB' e b = completionCB e b >>= \result ->
                                  case result of
                                    IOSuccess val -> signalReturn val
                                    IOFailed  err -> signalThrow err
        fptr <- FFI.allocOverlapped offset
        let lpol = unsafeForeignPtrToPtr fptr

        startCB lpol `onException` (Just `fmap` Win32.getLastError) >>= \result ->
          case result of
            Nothing -> do
                bytes <- FFI.getOverlappedResult h lpol True
                case bytes of
                  Just num_bytes -> completionCB' 0 num_bytes
                  Nothing        -> do err <- FFI.overlappedIOStatus lpol
                                       completionCB' err 0
            Just err -> do
                signalThrow (Just err)

-- Safe version of function
withOverlapped :: String
               -> HANDLE
               -> Word64 -- ^ Value to use for the @OVERLAPPED@
                         --   structure's Offset/OffsetHigh members.
               -> StartCallback (Maybe Int)
               -> CompletionCallback (IOResult a)
               -> IO (IOResult a)
withOverlapped fname h offset startCB completionCB
  = do mngr <- getSystemManager
       case mngr of
         Nothing    -> withOverlappedNonThreaded_    fname h offset startCB completionCB
         Just mngr' -> withOverlappedThreaded_ mngr' fname h offset startCB completionCB

withOverlappedEx :: Maybe Manager
                 -> String
                 -> HANDLE
                 -> Word64 -- ^ Value to use for the @OVERLAPPED@
                           --   structure's Offset/OffsetHigh members.
                 -> StartCallback (Maybe Int)
                 -> CompletionCallback (IOResult a)
                 -> IO (IOResult a)
withOverlappedEx mngr fname h offset startCB completionCB
  = do case mngr of
         Nothing    -> withOverlappedNonThreaded_    fname h offset startCB completionCB
         Just mngr' -> withOverlappedThreaded_ mngr' fname h offset startCB completionCB

------------------------------------------------------------------------
-- I/O Utilities

withException :: String -> IO (IOResult a) -> IO a
withException name fn
 = do res <- fn
      case res of
       IOSuccess a         -> return a
       IOFailed (Just err) -> FFI.throwWinErr name $ fromIntegral err
       IOFailed Nothing    -> FFI.throwWinErr name 0

ioSuccess :: a -> IO (IOResult a)
ioSuccess = return . IOSuccess

ioFailed :: Integral a => a -> IO (IOResult a)
ioFailed = return . IOFailed . Just . fromIntegral

------------------------------------------------------------------------
-- Timeouts

-- | A priority search queue, with timeouts as priorities.
type TimeoutQueue = Q.PSQ TimeoutCallback

-- |
-- Warning: since the 'TimeoutCallback' is called from the I/O manager, it must
-- not throw an exception or block for a long period of time.  In particular,
-- be wary of 'Control.Exception.throwTo' and 'Control.Concurrent.killThread':
-- if the target thread is making a foreign call, these functions will block
-- until the call completes.
type TimeoutCallback = IO ()

-- | An edit to apply to a 'TimeoutQueue'.
type TimeoutEdit = TimeoutQueue -> TimeoutQueue

-- | A timeout registration cookie.
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
    if relTime <= 0 then cb
    else do
      now <- getTime mgrClock
      let !expTime = now + relTime
      editTimeouts mgr (Q.insert key expTime cb)
      -- TODO: wakeManager mgr
    return $ TK key

-- | Update an active timeout to fire in the given number of seconds (from the
-- time 'updateTimeout' is called), instead of when it was going to fire.
-- This has no effect if the timeout has already fired.
updateTimeout :: Manager -> TimeoutKey -> Seconds -> IO ()
updateTimeout mgr (TK key) relTime = do
    now <- getTime (mgrClock mgr)
    let !expTime = now + relTime
    editTimeouts mgr (Q.adjust (const expTime) key)
    -- TODO: wakeManager mgr

-- | Unregister an active timeout.  This is a harmless no-op if the timeout is
-- already unregistered or has already fired.
--
-- Warning: the timeout callback may fire even after
-- 'unregisterTimeout' completes.
unregisterTimeout :: Manager -> TimeoutKey -> IO ()
unregisterTimeout mgr (TK key) =
    editTimeouts mgr (Q.delete key)
    -- TODO: wakeManager mgr

editTimeouts :: Manager -> TimeoutEdit -> IO ()
editTimeouts mgr g = atomicModifyIORef' (mgrTimeouts mgr) $ \tq -> (g tq, ())

------------------------------------------------------------------------
-- I/O manager loop

-- | Call all expired timeouts, and return how much time until the next
-- | expiration.
runExpiredTimeouts :: Manager -> IO (Maybe Seconds)
runExpiredTimeouts Manager{..} = do
    now <- getTime mgrClock
    (expired, delay) <- atomicModifyIORef' mgrTimeouts (mkTimeout now)
    -- Execute timeout callbacks.
    mapM_ Q.value expired
    return delay
      where
        mkTimeout :: Seconds -> TimeoutQueue ->
                     (TimeoutQueue, ([Q.Elem TimeoutCallback], Maybe Seconds))
        mkTimeout now tq =
           -- Remove timeouts with expiration <= now.
           let (expired, tq') = Q.atMost now tq in
           -- See how soon the next timeout expires.
           case Q.prio `fmap` Q.findMin tq' of
            Nothing ->
                (tq', (expired, Nothing))
            Just t ->
                -- This value will always be positive since the call
                -- to 'atMost' above removed any timeouts <= 'now'
                let !t' = t - now
                in (tq', (expired, Just t'))

-- | Return the delay argument to pass to GetQueuedCompletionStatus.
fromTimeout :: Maybe Seconds -> Word32
fromTimeout Nothing                 = 120000
fromTimeout (Just sec) | sec > 120  = 120000
                       | sec > 0    = ceiling (sec * 1000)
                       | otherwise  = 0

step :: Manager -> IO ()
step mgr@Manager{..} = do
    delay <- runExpiredTimeouts mgr
    n <- FFI.getQueuedCompletionStatusEx mgrIOCP mgrOverlappedEntries
         (fromTimeout delay)
    when (n > 0) $ do
      A.forM_ mgrOverlappedEntries $ \oe -> do
          mCD <- withMVar (callbackTableVar mgr (lpOverlapped oe)) $ \tbl ->
                   IT.delete (lpoverlappedToInt (lpOverlapped oe)) tbl
          case mCD of
            Nothing                        -> return ()
            Just (CompletionData _fptr cb) -> do
                         status <- FFI.overlappedIOStatus (lpOverlapped oe)
                         cb status (dwNumberOfBytesTransferred oe)
      cap <- A.capacity mgrOverlappedEntries
      when (cap == n) $ A.ensureCapacity mgrOverlappedEntries (2*cap)

loop :: Manager -> IO ()
loop mgr = go
    where
      go = do step mgr
              go
