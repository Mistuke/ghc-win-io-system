{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module GHC.Event.Windows.FFI (
    -- * IOCP
    IOCP(..),
    CompletionKey,
    newIOCP,
    associateHandleWithIOCP,
    getQueuedCompletionStatusEx,
    postQueuedCompletionStatus,
    getOverlappedResult,

    -- * Overlapped
    OVERLAPPED,
    LPOVERLAPPED,
    OVERLAPPED_ENTRY(..),
    LPOVERLAPPED_ENTRY,
    allocOverlapped,
    zeroOverlapped,
    pokeOffsetOverlapped,
    overlappedIOStatus,

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

#include <ntstatus.h>
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
import Foreign
import Foreign.C.Types
import GHC.Base
import GHC.Enum (fromEnum)
import GHC.Real (fromIntegral)
import GHC.Show
import GHC.Windows
import qualified GHC.Event.Array    as A
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

foreign import WINDOWS_CCONV safe "windows.h GetOverlappedResult"
    c_GetOverlappedResult :: HANDLE -> LPOVERLAPPED -> Ptr DWORD -> BOOL
                          -> IO BOOL

-- | Get the result of a single overlap operation without the IO manager
getOverlappedResult :: HANDLE -> Ptr OVERLAPPED -> BOOL -> IO (Maybe DWORD)
getOverlappedResult handle lp block
  = alloca $ \bytes ->
        do res <- c_GetOverlappedResult handle lp bytes block
           if res
              then fmap Just $ peek bytes
              else return Nothing

foreign import WINDOWS_CCONV safe "windows.h GetQueuedCompletionStatusEx"
    c_GetQueuedCompletionStatusEx :: IOCP -> LPOVERLAPPED_ENTRY -> Word32
                                  -> Ptr ULONG -> DWORD -> BOOL -> IO BOOL

getQueuedCompletionStatusEx :: IOCP
                            -> A.Array OVERLAPPED_ENTRY
                            -> DWORD  -- ^ Timeout in milliseconds (or
                                      -- 'GHC.Windows.iNFINITE')
                            -> IO Int
getQueuedCompletionStatusEx iocp arr timeout =
    alloca $ \num_removed_ptr ->do
        A.unsafeLoad arr $ \oes cap -> do
            ok <- c_GetQueuedCompletionStatusEx iocp oes (fromIntegral cap)
                  num_removed_ptr timeout False
            if ok then fromEnum `fmap` peek num_removed_ptr
            else do err <- getLastError
                    if err == #{const WAIT_TIMEOUT} then return 0
                    else failWith "GetQueuedCompletionStatusEx" err

overlappedIOStatus :: LPOVERLAPPED -> IO ErrCode
overlappedIOStatus lpol = do
  status <- #{peek OVERLAPPED, Internal} lpol
  if status == #{const STATUS_SUCCESS} then return #{const ERROR_SUCCESS}
  -- TODO: Map NTSTATUS to ErrCode?
  -- See https://github.com/libuv/libuv/blob/b12624c13693c4d29ca84b3556eadc9e9c0936a4/src/win/winsock.c#L153
  else return status
{-# INLINE overlappedIOStatus #-}

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

-- | An array of these is passed to GetQueuedCompletionStatusEx as an output
-- argument.
data OVERLAPPED_ENTRY = OVERLAPPED_ENTRY {
      lpCompletionKey            :: ULONG_PTR,
      lpOverlapped               :: LPOVERLAPPED,
      dwNumberOfBytesTransferred :: DWORD
    }

type LPOVERLAPPED_ENTRY = Ptr OVERLAPPED_ENTRY

instance Storable OVERLAPPED_ENTRY where
    sizeOf _    = #{size OVERLAPPED_ENTRY}
    alignment _ = alignment (undefined :: CInt)

    peek ptr = do
      lpCompletionKey <- #{peek OVERLAPPED_ENTRY, lpCompletionKey} ptr
      lpOverlapped    <- #{peek OVERLAPPED_ENTRY, lpOverlapped} ptr
      dwNumberOfBytesTransferred <-
          #{peek OVERLAPPED_ENTRY, dwNumberOfBytesTransferred} ptr
      let !oe = OVERLAPPED_ENTRY{..}
      return oe

    poke ptr OVERLAPPED_ENTRY{..} = do
      #{poke OVERLAPPED_ENTRY, lpCompletionKey} ptr lpCompletionKey
      #{poke OVERLAPPED_ENTRY, lpOverlapped} ptr lpOverlapped
      #{poke OVERLAPPED_ENTRY, dwNumberOfBytesTransferred}
        ptr dwNumberOfBytesTransferred

-- | Allocate a new
-- <http://msdn.microsoft.com/en-us/library/windows/desktop/ms684342%28v=vs.85%29.aspx
-- OVERLAPPED> structure.
allocOverlapped :: Word64 -- ^ Offset/OffsetHigh
                -> IO (ForeignPtr OVERLAPPED)
allocOverlapped offset = do
  fptr <- mallocForeignPtrBytes #{size OVERLAPPED}
  withForeignPtr fptr $ \lpol ->
      do zeroOverlapped lpol
         pokeOffsetOverlapped lpol offset
  return fptr

-- | Zero-fill an OVERLAPPED structure.
zeroOverlapped :: LPOVERLAPPED -> IO ()
zeroOverlapped lpol = fillBytes lpol 0 #{size OVERLAPPED}
{-# INLINE zeroOverlapped #-}

-- | Set the offset field in an OVERLAPPED structure.
pokeOffsetOverlapped :: LPOVERLAPPED -> Word64 -> IO ()
pokeOffsetOverlapped lpol offset = do
  let (offsetLow, offsetHigh) = Win32.ddwordToDwords offset
  #{poke OVERLAPPED, Offset} lpol offsetLow
  #{poke OVERLAPPED, OffsetHigh} lpol offsetHigh
{-# INLINE pokeOffsetOverlapped #-}

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

type ULONG      = #type ULONG
type ULONG_PTR  = #type ULONG_PTR

throwWinErr :: String -> ErrCode -> IO a
throwWinErr loc err = do
    c_SetLastError err
    Win32.failWith loc err

foreign import WINDOWS_CCONV unsafe "windows.h SetLastError"
    c_SetLastError :: ErrCode -> IO ()
