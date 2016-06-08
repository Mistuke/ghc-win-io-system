{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module System.IO.IOCP (openFileIOCP, readFileIOCP, closeFileIOCP)
    where

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

import Prelude hiding (readFile)
import Control.Monad (when)
import Data.ByteString hiding (readFile)
import Data.ByteString.Internal (createAndTrim)
import GHC.Windows
import GHC.Event.Windows (LPOVERLAPPED, associateHandle, withOverlapped)
import Foreign.Ptr
import System.Win32.Types (LPCTSTR, LPVOID, LPDWORD, nullHANDLE, nullPtr)
import qualified System.Win32.Types as Win32
import qualified GHC.Event.Windows as Mgr
import qualified GHC.Event.Windows.FFI as FFI

type LPSECURITY_ATTRIBUTES = LPVOID

getManager :: IO Mgr.Manager
getManager = Mgr.getSystemManager >>= maybe (fail "requires threaded RTS") return

openFileIOCP :: FilePath -> IO HANDLE
openFileIOCP fp = do mgr <- getManager
                     h <- createFile
                     associateHandle mgr h
                     return h
    where
      createFile =
          Win32.withTString fp $ \fp' ->
              failIf (== iNVALID_HANDLE_VALUE) "CreateFile failed" $
                     c_CreateFile fp' #{const GENERIC_READ}
                                      #{const FILE_SHARE_READ}
                                      nullPtr
                                      #{const OPEN_EXISTING}
                                      #{const FILE_FLAG_OVERLAPPED}
                                      nullHANDLE


readFileIOCP :: HANDLE -> IO ByteString
readFileIOCP h = do mgr <- getManager
                    readFile mgr
    where
      bufSize = 5120

      readFile mgr = createAndTrim (fromIntegral bufSize) $ \outBuf ->
                     withOverlapped mgr h 0 (startCB outBuf) completionCB
          where
            startCB outBuf lpOverlapped = do
              ret <-
                c_ReadFile h (castPtr outBuf) bufSize nullPtr lpOverlapped
              when (not ret) $
                   failIf_ (/= #{const ERROR_IO_PENDING}) "ReadFile failed" $
                           Win32.getLastError

            completionCB err dwBytes
                | err == 0  = return (fromIntegral dwBytes)
                | otherwise = FFI.throwWinErr "readFile" err

closeFileIOCP :: HANDLE -> IO ()
closeFileIOCP h = failIfFalse_ "ClosHandle failed!" $ c_CloseHandle h

foreign import WINDOWS_CCONV unsafe "windows.h CreateFileW"
    c_CreateFile :: LPCTSTR -> DWORD -> DWORD -> LPSECURITY_ATTRIBUTES
                 -> DWORD -> DWORD -> HANDLE
                 -> IO HANDLE

foreign import WINDOWS_CCONV unsafe "windows.h CloseHandle"
    c_CloseHandle :: HANDLE -> IO BOOL

foreign import WINDOWS_CCONV unsafe "windows.h ReadFile"
    c_ReadFile :: HANDLE -> LPVOID -> DWORD -> LPDWORD -> LPOVERLAPPED
               -> IO BOOL
