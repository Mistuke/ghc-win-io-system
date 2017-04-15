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
   test,
   test2,
   -- * Standard Handles
   stdin,
   stdout,
   stderr
 ) where

#include <windows.h>
##include "windows_cconv.h"

import Prelude hiding (readFile)
import Control.Monad (when)
import Data.ByteString hiding (readFile)
import qualified Data.ByteString as BS
import Data.ByteString.Internal (createAndTrim)
import Data.Word (Word8)

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
import GHC.Event.Windows (LPOVERLAPPED, associateHandle, withOverlapped,
                          withOverlapped_, IOResult(..))
import Foreign.Ptr
import Foreign.Marshal.Array (allocaArray)
import qualified GHC.Event.Windows as Mgr
import qualified GHC.Event.Windows.FFI as FFI

import System.Win32.Types (LPCTSTR, LPVOID, LPDWORD, DWORD, HANDLE, BOOL,
                           nullHANDLE, failIf, iNVALID_HANDLE_VALUE,
                           failIfFalse_, failIf_)
import qualified System.Win32.Types as Win32

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
  read             = hwndRead
  readNonBlocking  = hwndReadNonBlocking
  write            = hwndWrite
  writeNonBlocking = hwndWriteNonBlocking

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
-- See libraries/base/GHC/IO/BufferedIO.hs
instance BufferedIO Handle where
  newBuffer _dev state = newByteBuffer dEFAULT_BUFFER_SIZE state
  fillReadBuffer       = readBuf
  fillReadBuffer0      = readBufNonBlocking
  flushWriteBuffer     = writeBuf
  flushWriteBuffer0    = writeBufNonBlocking

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
-- Reading and Writing

-- For this to actually block, the file handle must have
-- been created with FILE_FLAG_OVERLAPPED not set.
hwndRead :: Handle -> Ptr Word8 -> Int -> IO Int
hwndRead hwnd ptr bytes
  = do mgr <- getManager
       withOverlapped mgr (handle hwnd) 0 (startCB ptr) completionCB
  where
    startCB outBuf lpOverlapped = do
      ret <- c_ReadFile (handle hwnd) (castPtr outBuf) (fromIntegral bytes)
                        nullPtr lpOverlapped
      when (not ret) $
            failIf_ (/= #{const ERROR_IO_PENDING}) "ReadFile failed" $
                    Win32.getLastError

    completionCB err dwBytes
        | err == 0  = return (fromIntegral dwBytes)
        | otherwise = FFI.throwWinErr "readFileRaw" err

-- For this to actually block, the file handle must have
-- been created with FILE_FLAG_OVERLAPPED set.
-- TODO: create non-rts version.
hwndReadNonBlocking :: Handle -> Ptr Word8 -> Int -> IO (Maybe Int)
hwndReadNonBlocking hwnd ptr bytes
  = do mgr <- getManager
       val <- withOverlapped_ mgr "hwndReadNonBlocking" (handle hwnd) 0
                              (startCB ptr) completionCB
       return $ Just $ ioValue val
  where
    startCB outBuf lpOverlapped = do
      ret <- c_ReadFile (handle hwnd) (castPtr outBuf) (fromIntegral bytes)
                        nullPtr lpOverlapped
      err <- fmap fromIntegral Win32.getLastError
      if   not ret
        && (err == #{const ERROR_IO_PENDING}
            || err == #{const ERROR_HANDLE_EOF})
        then return Nothing
        else return (Just err)

    completionCB err dwBytes
        | err == 0  = return $ IOSuccess $ fromIntegral dwBytes
        | otherwise = return $ IOFailed $ Just $ fromIntegral err

hwndWrite :: Handle -> Ptr Word8 -> Int -> IO ()
hwndWrite handle ptr bytes = undefined

hwndWriteNonBlocking :: Handle -> Ptr Word8 -> Int -> IO Int
hwndWriteNonBlocking handle ptr bytes = undefined

-- -----------------------------------------------------------------------------
-- opening files

test :: IO Int
test = do hwnd <- openFile "r:\\hello.txt"
          bytes <- allocaArray 50 $ \ptr -> hwndRead hwnd ptr 50
          closeFile hwnd
          return bytes

test2 :: IO (Maybe Int)
test2 = do hwnd <- openFile "r:\\hello.txt"
           bytes <- allocaArray 50 $ \ptr -> hwndReadNonBlocking hwnd ptr 50
           closeFile hwnd
           return bytes

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

closeFile :: Handle -> IO ()
closeFile h = failIfFalse_ "ClosHandle failed!" $ c_CloseHandle $ handle h