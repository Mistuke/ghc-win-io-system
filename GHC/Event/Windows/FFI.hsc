{-# LANGUAGE CPP #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}
module GHC.Event.Windows.FFI (
    -- * IOCP
    IOCP(..),
    CompletionKey,
    newIOCP,
    associateHandleWithIOCP,
    getNextCompletion,
    postQueuedCompletionStatus,

    -- * Overlapped
    OVERLAPPED,
    LPOVERLAPPED,
    newOverlapped,

    -- * Cancel pending I/O
    cancelIoEx,

    -- * Monotonic time

    -- ** GetTickCount
    getTickCount64,

    -- ** QueryPerformanceCounter
    queryPerformanceCounter,
    queryPerformanceFrequency,

    -- ** miscellaneous
    throwWinErr,
) where

#include <windows.h>

##ifdef mingw32_HOST_OS
## if defined(i386_HOST_ARCH)
##  define WINDOWS_CCONV stdcall
## elif defined(x86_64_HOST_ARCH)
##  define WINDOWS_CCONV ccall
## else
##  error Unknown mingw32 arch
## endif
##endif

import Data.Maybe
import Data.Word
import Foreign
import GHC.Base
import GHC.Show
import GHC.Windows
import System.Win32.Types (LPDWORD)
import qualified System.Win32.Types as Win32

------------------------------------------------------------------------
-- IOCP

-- | An I/O completion port.
newtype IOCP = IOCP HANDLE
    deriving (Eq, Ord, Show)

type CompletionKey = ULONG_PTR

foreign import WINDOWS_CCONV unsafe "windows.h CreateIoCompletionPort"
    c_CreateIoCompletionPort :: HANDLE -> IOCP -> ULONG_PTR -> DWORD
                             -> IO IOCP

-- | Create a new I/O completion port.
newIOCP :: IO IOCP
newIOCP = failIf (== IOCP nullPtr) "newIOCP" $
          c_CreateIoCompletionPort iNVALID_HANDLE_VALUE (IOCP nullPtr) 0 1

-- | Associate a HANDLE with an I/O completion port.
associateHandleWithIOCP :: IOCP -> HANDLE -> CompletionKey -> IO ()
associateHandleWithIOCP iocp handle completionKey =
    failIf_ (/= iocp) "associateHandleWithIOCP" $
        c_CreateIoCompletionPort handle iocp completionKey 0

foreign import WINDOWS_CCONV safe "windows.h GetQueuedCompletionStatus"
    c_GetQueuedCompletionStatus :: IOCP -> LPDWORD -> PULONG_PTR
                                -> Ptr LPOVERLAPPED -> DWORD -> IO BOOL

getNextCompletion :: IOCP
                  -> DWORD  -- ^ Timeout in milliseconds (or
                            -- 'GHC.Windows.iNFINITE')
                  -> IO (Maybe (LPOVERLAPPED, ErrCode, DWORD))
getNextCompletion iocp timeout =
    alloca $ \num_bytes_ptr ->
    alloca $ \completion_key_ptr ->
    alloca $ \lpoverlapped_ptr -> do
        ok <- c_GetQueuedCompletionStatus iocp num_bytes_ptr completion_key_ptr
              lpoverlapped_ptr timeout
        err <- if ok then return #{const ERROR_SUCCESS}
               else getLastError
        if ok then do
            lpol      <- peek lpoverlapped_ptr
            num_bytes <- peek num_bytes_ptr
            return $ Just (lpol, err, num_bytes)
        else if err == #{const WAIT_TIMEOUT} then
            return Nothing
        else
            failWith "GetQueuedCompletionStatus" err

foreign import WINDOWS_CCONV unsafe "windows.h PostQueuedCompletionStatus"
    c_PostQueuedCompletionStatus :: IOCP -> DWORD -> ULONG_PTR -> LPOVERLAPPED
                                 -> IO BOOL

postQueuedCompletionStatus :: IOCP -> DWORD -> CompletionKey -> LPOVERLAPPED
                           -> IO ()
postQueuedCompletionStatus iocp numBytes completionKey lpol =
    failIfFalse_ "PostQueuedCompletionStatus" $
    c_PostQueuedCompletionStatus iocp numBytes completionKey lpol

------------------------------------------------------------------------
-- Overlapped

-- | Tag type for @LPOVERLAPPED@.
data OVERLAPPED

-- | Identifies an I/O operation.  Used as the @LPOVERLAPPED@ parameter
-- for overlapped I/O functions (e.g. @ReadFile@, @WSASend@).
type LPOVERLAPPED = Ptr OVERLAPPED

-- | Allocate a new
-- <http://msdn.microsoft.com/en-us/library/windows/desktop/ms684342%28v=vs.85%29.aspx
-- OVERLAPPED> structure.
newOverlapped :: Word64 -- ^ Offset/OffsetHigh
              -> IO (ForeignPtr OVERLAPPED)
newOverlapped offset = do
  fptr <- mallocForeignPtrBytes #{size OVERLAPPED}
  withForeignPtr fptr $ \p ->
      do fillBytes p 0 #{size OVERLAPPED}
         let (offsetLow, offsetHigh) = Win32.ddwordToDwords offset
         #{poke OVERLAPPED, Offset} p offsetLow
         #{poke OVERLAPPED, OffsetHigh} p offsetHigh
  return fptr

------------------------------------------------------------------------
-- Cancel pending I/O

-- | CancelIo shouldn't block, but cancellation happens infrequently,
-- so we might as well be on the safe side.
foreign import WINDOWS_CCONV safe "windows.h CancelIoEx"
    c_CancelIoEx :: HANDLE -> LPOVERLAPPED -> IO BOOL

-- | Cancel all pending overlapped I/O for the given file that was initiated by
-- the current OS thread.
cancelIoEx :: HANDLE -> LPOVERLAPPED -> IO ()
cancelIoEx h o = failIfFalse_ "CancelIoEx" . c_CancelIoEx h $ o

------------------------------------------------------------------------
-- Monotonic time

foreign import WINDOWS_CCONV "windows.h GetTickCount64"
    c_GetTickCount64 :: IO #{type ULONGLONG}

-- | Call the @GetTickCount64@ function, which returns a monotonic time in
-- milliseconds.
--
-- Problems:
--
--  * Low resolution (10 to 16 milliseconds).
--
-- <http://msdn.microsoft.com/en-us/library/windows/desktop/ms724408%28v=vs.85%29.aspx>
getTickCount64 :: IO Word64
getTickCount64 = c_GetTickCount64

-- | Call the @QueryPerformanceCounter@ function.
--
-- Problems:
--
--  * Might not be available on some hardware.  Use 'queryPerformanceFrequency'
--    to test for availability before calling this function.
--
--  * On a multiprocessor computer, may produce different results on
--    different processors due to hardware bugs.
--
-- To get a monotonic time in seconds, divide the result of
-- 'queryPerformanceCounter' by that of 'queryPerformanceFrequency'.
--
-- <http://msdn.microsoft.com/en-us/library/windows/desktop/ms644904%28v=vs.85%29.aspx>
queryPerformanceCounter :: IO Int64
queryPerformanceCounter =
    callQP c_QueryPerformanceCounter
    >>= maybe (throwGetLastError "QueryPerformanceCounter") return

-- | Call the @QueryPerformanceFrequency@ function.  Return 'Nothing' if the
-- hardware does not provide a high-resolution performance counter.
--
-- <http://msdn.microsoft.com/en-us/library/windows/desktop/ms644905%28v=vs.85%29.aspx>
queryPerformanceFrequency :: IO (Maybe Int64)
queryPerformanceFrequency = do
    m <- callQP c_QueryPerformanceFrequency
    case m of
        Nothing   -> return Nothing
        Just 0    -> return Nothing -- Shouldn't happen; just a safeguard to
                                    -- avoid a zero denominator.
        Just freq -> return (Just freq)

type QPFunc = Ptr Int64 -> IO BOOL

foreign import WINDOWS_CCONV "Windows.h QueryPerformanceCounter"
    c_QueryPerformanceCounter :: QPFunc

foreign import WINDOWS_CCONV "Windows.h QueryPerformanceFrequency"
    c_QueryPerformanceFrequency :: QPFunc

callQP :: QPFunc -> IO (Maybe Int64)
callQP qpfunc =
    allocaBytes #{size LARGE_INTEGER} $ \ptr -> do
        ok <- qpfunc ptr
        if ok then do
            n <- #{peek LARGE_INTEGER, QuadPart} ptr
            return (Just n)
        else
            return Nothing

------------------------------------------------------------------------
-- Miscellaneous

type ULONG_PTR  = #type ULONG_PTR
type PULONG_PTR = Ptr ULONG_PTR

throwWinErr :: String -> ErrCode -> IO a
throwWinErr loc err = do
    c_SetLastError err
    Win32.failWith loc err

foreign import WINDOWS_CCONV unsafe "windows.h SetLastError"
    c_SetLastError :: ErrCode -> IO ()
