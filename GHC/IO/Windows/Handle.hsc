{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
-- Whether there are identities depends on the platform
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Windows.Handle
-- Copyright   :  (c) The University of Glasgow, 2017
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Raw read/write operations on Windows Handles
--
-----------------------------------------------------------------------------

module GHC.IO.Windows.Handle
 ( -- * Basic Types
   Handle(..),

   -- * Standard Handles
   stdin,
   stdout,
   stderr
 ) where

#include <windows.h>
##include "windows_cconv.h"

import GHC.Base
import GHC.Num
import GHC.Real
import GHC.Show
import GHC.Enum

import GHC.IO
import GHC.IO.IOMode
import GHC.IO.Buffer
import GHC.IO.BufferedIO
import qualified GHC.IO.Device
import GHC.IO.Device (SeekMode(..), IODeviceType(..))
import GHC.Event.Windows (LPOVERLAPPED, associateHandle, withOverlapped)
import Foreign.Ptr

import System.Win32.Types

c_DEBUG_DUMP :: Bool
c_DEBUG_DUMP = False

-- -----------------------------------------------------------------------------
-- The Windows IO device

newtype Handle = Handle { handle :: HANDLE }

-- | Create a new Handle object
mkHandle :: HANDLE -> Handle
mkHandle = Handle

-- | @since 4.11.0.0
instance Show Handle where
  show = show . handle

-- | @since 4.11.0.0
instance GHC.IO.Device.RawIO Handle where
  --read             = fdRead
  --readNonBlocking  = fdReadNonBlocking
  --write            = fdWrite
  --writeNonBlocking = fdWriteNonBlocking

-- | @since 4.11.0.0
instance GHC.IO.Device.IODevice Handle where
  --ready         = ready
  --close         = close
  --isTerminal    = isTerminal
  --isSeekable    = isSeekable
  --seek          = seek
  --tell          = tell
  --getSize       = getSize
  --setSize       = setSize
  --setEcho       = setEcho
  --getEcho       = getEcho
  --setRaw        = setRaw
  --devType       = devType
  --dup           = dup
  --dup2          = dup2

-- Default sequential read buffer size.
dEFAULT_BUFFER_SIZE :: Int
dEFAULT_BUFFER_SIZE = 8192

-- | @since 4.11.0.0
instance BufferedIO Handle where
  newBuffer _dev state = newByteBuffer dEFAULT_BUFFER_SIZE state
  fillReadBuffer    hwnd buf = readBuf' hwnd buf
  fillReadBuffer0   hwnd buf = readBufNonBlocking hwnd buf
  flushWriteBuffer  hwnd buf = writeBuf' hwnd buf
  flushWriteBuffer0 hwnd buf = writeBufNonBlocking hwnd buf

readBuf' :: Handle -> Buffer Word8 -> IO (Int, Buffer Word8)
readBuf' hwnd buf = do
  when c_DEBUG_DUMP $
      puts ("readBuf fd=" ++ show fd ++ " " ++ summaryBuffer buf ++ "\n")
  (r,buf') <- readBuf fd buf
  when c_DEBUG_DUMP $
      puts ("after: " ++ summaryBuffer buf' ++ "\n")
  return (r,buf')

writeBuf' :: Handle -> Buffer Word8 -> IO (Buffer Word8)
writeBuf' hwnd buf = do
  when c_DEBUG_DUMP $
      puts ("writeBuf fd=" ++ show fd ++ " " ++ summaryBuffer buf ++ "\n")
  writeBuf hwnd buf

-- -----------------------------------------------------------------------------
-- Standard I/O handles

type StdHandleId   = DWORD

#{enum StdHandleId,
 , sTD_INPUT_HANDLE  = STD_INPUT_HANDLE
 , sTD_OUTPUT_HANDLE = STD_OUTPUT_HANDLE
 , sTD_ERROR_HANDLE  = STD_ERROR_HANDLE
}

getStdHandle :: StdHandleId -> IO HANDLE
getStdHandle hid =
  failIf (== iNVALID_HANDLE_VALUE) "GetStdHandle" $ c_GetStdHandle hid

foreign import WINDOWS_CCONV unsafe "windows.h GetStdHandle"
    c_GetStdHandle :: StdHandleId -> IO HANDLE

stdin, stdout, stderr :: IO HANDLE
stdin  = getStdHandle sTD_INPUT_HANDLE
stdout = getStdHandle sTD_OUTPUT_HANDLE
stderr = getStdHandle sTD_ERROR_HANDLE

-- -----------------------------------------------------------------------------
-- Foreign imports

foreign import WINDOWS_CCONV unsafe "windows.h CreateFileW"
    c_CreateFile :: LPCTSTR -> DWORD -> DWORD -> LPSECURITY_ATTRIBUTES
                 -> DWORD -> DWORD -> HANDLE
                 -> IO HANDLE

foreign import WINDOWS_CCONV unsafe "windows.h CloseHandle"
    c_CloseHandle :: HANDLE -> IO BOOL

foreign import WINDOWS_CCONV unsafe "windows.h ReadFile"
    c_ReadFile :: HANDLE -> LPVOID -> DWORD -> LPDWORD -> LPOVERLAPPED
               -> IO BOOL

type LPSECURITY_ATTRIBUTES = LPVOID

-- -----------------------------------------------------------------------------
-- I/O Manager

getManager :: IO Mgr.Manager
getManager = Mgr.getSystemManager >>= maybe (fail "requires threaded RTS") return

-- -----------------------------------------------------------------------------
-- opening files

openFile :: FilePath -> IO Handle
openFile fp = do mgr <- getManager
                     h <- createFile
                     associateHandle mgr h
                     return $ mkHandle h
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

-- -----------------------------------------------------------------------------
-- Operations on file descriptors

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