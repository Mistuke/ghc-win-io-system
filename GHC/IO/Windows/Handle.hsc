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
   Handle(),
   ConsoleHandle(),
   test,
   test2,
   test3,
   -- * Standard Handles
   stdin,
   stdout,
   stderr
 ) where

#include <windows.h>
##include "windows_cconv.h"

import Prelude hiding (readFile)
import Data.Bits ((.|.))
import Data.Word (Word8)

import GHC.Base

import GHC.IO.Buffer
import GHC.IO.BufferedIO
import qualified GHC.IO.Device
import GHC.IO.Device (SeekMode(..), IODeviceType(..))
import GHC.Event.Windows (LPOVERLAPPED, associateHandle', withOverlapped,
                          IOResult(..))
import Foreign.Ptr
import Foreign.C
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, withArray)
import Foreign.Storable (peek)
import qualified GHC.Event.Windows as Mgr

import System.Win32.Types (LPCTSTR, LPVOID, LPDWORD, DWORD, HANDLE, BOOL,
                           nullHANDLE, failIf, iNVALID_HANDLE_VALUE,
                           failIfFalse_, failIf_)
import qualified System.Win32.Types as Win32

-- -----------------------------------------------------------------------------
-- The Windows IO device handles

newtype Handle = Handle { getHandle :: HANDLE }
newtype ConsoleHandle = ConsoleHandle { getConsoleHandle :: HANDLE }

-- | Convert a ConsoleHandle into a general FileHandle
--   This will change which DeviceIO is used.
convertHandle :: ConsoleHandle -> Handle
convertHandle = fromHANDLE . toHANDLE

-- | @since 4.11.0.0
instance Show Handle where
  show = show . getHandle

-- | @since 4.11.0.0
instance Show ConsoleHandle where
  show = show . getConsoleHandle

-- | @since 4.11.0.0
instance GHC.IO.Device.RawIO Handle where
  read             = hwndRead
  readNonBlocking  = hwndReadNonBlocking
  write            = hwndWrite
  writeNonBlocking = hwndWriteNonBlocking

-- | Generalize a way to get and create handles.
class RawHandle a where
  toHANDLE   :: a -> HANDLE
  fromHANDLE :: HANDLE -> a

instance RawHandle Handle where
  toHANDLE   = getHandle
  fromHANDLE = Handle

instance RawHandle ConsoleHandle where
  toHANDLE   = getConsoleHandle
  fromHANDLE = ConsoleHandle

-- -----------------------------------------------------------------------------
-- The Windows IO device implementation

-- | @since 4.11.0.0
instance GHC.IO.Device.IODevice Handle where
  ready      = handle_ready
  close      = handle_close
  isTerminal = handle_is_console
  isSeekable = handle_is_seekable
  seek       = handle_seek
  tell       = handle_tell
  getSize    = handle_get_size
  setSize    = handle_set_size
  setEcho    = handle_set_echo
  getEcho    = handle_get_echo
  setRaw     = handle_set_buffering
  devType    = handle_dev_type
  dup        = handle_duplicate

-- | @since 4.11.0.0
instance GHC.IO.Device.IODevice ConsoleHandle where
  ready      = handle_ready
  close      = handle_close . convertHandle
  isTerminal = handle_is_console
  isSeekable = handle_is_seekable
  seek       = handle_console_seek
  tell       = handle_console_tell
  getSize    = handle_get_console_size
  setSize    = handle_set_console_size
  setEcho    = handle_set_echo
  getEcho    = handle_get_echo
  setRaw     = handle_set_buffering
  devType    = handle_dev_type
  dup        = handle_duplicate

-- Default sequential read buffer size.
-- for Windows 8k seems to be the optimal
-- buffer size.
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

stdin, stdout, stderr :: IO ConsoleHandle
stdin  = fromHANDLE <$> getStdHandle sTD_INPUT_HANDLE
stdout = fromHANDLE <$> getStdHandle sTD_OUTPUT_HANDLE
stderr = fromHANDLE <$> getStdHandle sTD_ERROR_HANDLE

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

foreign import WINDOWS_CCONV unsafe "windows.h WriteFile"
    c_WriteFile :: HANDLE -> LPVOID -> DWORD -> LPDWORD -> LPOVERLAPPED
                -> IO BOOL

foreign import WINDOWS_CCONV unsafe "windows.h GetStdHandle"
    c_GetStdHandle :: StdHandleId -> IO HANDLE

foreign import ccall safe "__handle_ready"
    c_handle_ready :: HANDLE -> BOOL -> CInt -> IO CInt

foreign import ccall safe "__is_console"
    c_is_console :: HANDLE -> IO BOOL

foreign import ccall safe "__set_console_buffering"
    c_set_console_buffering :: HANDLE -> BOOL -> IO BOOL

foreign import ccall safe "__set_console_echo"
    c_set_console_echo :: HANDLE -> BOOL -> IO BOOL

foreign import ccall safe "__get_console_echo"
    c_get_console_echo :: HANDLE -> IO BOOL

foreign import ccall safe "__flush_input_console"
    c_flush_input_console :: HANDLE -> IO BOOL

foreign import ccall safe "__close_handle"
    c_close_handle :: HANDLE -> IO ()

foreign import ccall safe "__handle_type"
    c_handle_type :: HANDLE -> IO Int

foreign import ccall safe "__set_file_pointer"
  c_set_file_pointer :: HANDLE -> CLong -> DWORD -> IO BOOL

foreign import ccall safe "__get_file_pointer"
  c_get_file_pointer :: HANDLE -> IO CLong

foreign import ccall safe "__get_file_size"
  c_get_file_size :: HANDLE -> IO CLong

foreign import ccall safe "__set_file_size"
  c_set_file_size :: HANDLE -> CLong -> IO BOOL

foreign import ccall safe "__duplicate_handle"
  c_duplicate_handle :: HANDLE -> Ptr HANDLE -> IO BOOL

foreign import ccall safe "__set_console_pointer"
  c_set_console_pointer :: HANDLE -> CLong -> DWORD -> IO BOOL

foreign import ccall safe "__get_console_pointer"
  c_get_console_pointer :: HANDLE -> IO CLong

foreign import ccall safe "__get_console_buffer_size"
  c_get_console_buffer_size :: HANDLE -> IO CLong

foreign import ccall safe "__set_console_buffer_size"
  c_set_console_buffer_size :: HANDLE -> CLong -> IO BOOL

type LPSECURITY_ATTRIBUTES = LPVOID

-- -----------------------------------------------------------------------------
-- Reading and Writing

-- For this to actually block, the file handle must have
-- been created with FILE_FLAG_OVERLAPPED not set.
hwndRead :: Handle -> Ptr Word8 -> Int -> IO Int
hwndRead hwnd ptr bytes
  = do fmap fromIntegral $ Mgr.withException "hwndRead" $
          withOverlapped "hwndRead" (getHandle hwnd) 0 (startCB ptr) completionCB
  where
    startCB outBuf lpOverlapped = do
      ret <- c_ReadFile (getHandle hwnd) (castPtr outBuf) (fromIntegral bytes)
                        nullPtr lpOverlapped
      when (not ret) $
            failIf_ (/= #{const ERROR_IO_PENDING}) "ReadFile failed" $
                    Win32.getLastError
      return Nothing

    completionCB err dwBytes
        | err == 0  = Mgr.ioSuccess $ fromIntegral dwBytes
        | otherwise = Mgr.ioFailed err

-- There's no non-blocking file I/O on Windows I think..
-- But sockets etc should be possible.
-- Revisit this when implementing sockets and pipes.
hwndReadNonBlocking :: Handle -> Ptr Word8 -> Int -> IO (Maybe Int)
hwndReadNonBlocking hwnd ptr bytes
  = do val <- withOverlapped "hwndReadNonBlocking" (getHandle hwnd) 0
                              (startCB ptr) completionCB
       return $ Just $ fromIntegral $ ioValue val
  where
    startCB inputBuf lpOverlapped = do
      ret <- c_ReadFile (getHandle hwnd) (castPtr inputBuf) (fromIntegral bytes)
                        nullPtr lpOverlapped
      err <- fmap fromIntegral Win32.getLastError
      if not ret
        && (err == #{const ERROR_IO_PENDING}
            || err == #{const ERROR_HANDLE_EOF})
        then return Nothing
        else return (Just err)

    completionCB err dwBytes
        | err == 0  = Mgr.ioSuccess $ fromIntegral dwBytes
        | otherwise = Mgr.ioFailed err

hwndWrite :: Handle -> Ptr Word8 -> Int -> IO ()
hwndWrite hwnd ptr bytes
  = do _ <- Mgr.withException "hwndWrite" $
          withOverlapped "hwndWrite" (getHandle hwnd) 0 (startCB ptr) completionCB
       return ()
  where
    startCB outBuf lpOverlapped = do
      ret <- c_WriteFile (getHandle hwnd) (castPtr outBuf) (fromIntegral bytes)
                         nullPtr lpOverlapped
      when (not ret) $
            failIf_ (/= #{const ERROR_IO_PENDING}) "WriteFile failed" $
                    Win32.getLastError
      return Nothing

    completionCB err dwBytes
        | err == 0  = Mgr.ioSuccess $ fromIntegral dwBytes
        | otherwise = Mgr.ioFailed err

hwndWriteNonBlocking :: Handle -> Ptr Word8 -> Int -> IO Int
hwndWriteNonBlocking hwnd ptr bytes
  = do val <- withOverlapped "hwndReadNonBlocking" (getHandle hwnd) 0
                             (startCB ptr) completionCB
       return $ fromIntegral $ ioValue val
  where
    startCB outBuf lpOverlapped = do
      ret <- c_WriteFile (getHandle hwnd) (castPtr outBuf) (fromIntegral bytes)
                         nullPtr lpOverlapped
      err <- fmap fromIntegral Win32.getLastError

      if not ret
        && (err == #{const ERROR_IO_PENDING}
            || err == #{const ERROR_HANDLE_EOF})
        then return Nothing
        else return (Just err)

    completionCB err dwBytes
        | err == 0  = Mgr.ioSuccess $ fromIntegral dwBytes
        | otherwise = Mgr.ioFailed err

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

test3 :: IO Int
test3 = do hwnd <- openFile2 "r:\\hello2.txt"
           let vals = fmap fromIntegral ([1..30000] :: [Int])
           let num = Prelude.length vals
           bytes <- withArray vals $ \ptr -> hwndWrite hwnd ptr num
           closeFile hwnd
           return num

openFile :: FilePath -> IO Handle
openFile fp = do h <- createFile
                 associateHandle' h
                 return $ fromHANDLE h
    where
      createFile =
          Win32.withTString fp $ \fp' ->
              failIf (== iNVALID_HANDLE_VALUE) "CreateFile failed" $
                     c_CreateFile fp' #{const GENERIC_READ}
                                      #{const FILE_SHARE_READ}
                                      nullPtr
                                      #{const OPEN_EXISTING}
                                      (#{const FILE_FLAG_OVERLAPPED} .|. #{const FILE_FLAG_SEQUENTIAL_SCAN })
                                      nullHANDLE

openFile2 :: FilePath -> IO Handle
openFile2 fp = do h <- createFile
                  associateHandle' h
                  return $ fromHANDLE h
    where
      createFile =
          Win32.withTString fp $ \fp' ->
              failIf (== iNVALID_HANDLE_VALUE) "CreateFile failed" $
                     c_CreateFile fp' #{const GENERIC_WRITE}
                                      (#{const FILE_SHARE_WRITE} .|. #{const FILE_SHARE_READ})
                                      nullPtr
                                      #{const CREATE_ALWAYS}
                                      #{const FILE_FLAG_OVERLAPPED}
                                      nullHANDLE

-- -----------------------------------------------------------------------------
-- Operations on file handles

closeFile :: RawHandle a => a -> IO ()
closeFile = failIfFalse_ "CloseHandle failed!" . c_CloseHandle . toHANDLE

handle_ready :: RawHandle a => a -> Bool -> Int -> IO Bool
handle_ready hwnd write msecs = do
  r <- throwErrnoIfMinus1Retry "GHC.IO.Windows.Handle.handle_ready" $
          c_handle_ready (toHANDLE hwnd) write (fromIntegral msecs)
  return (toEnum (fromIntegral r))

handle_is_console :: RawHandle a => a -> IO Bool
handle_is_console = c_is_console . toHANDLE

handle_close :: RawHandle a => a -> IO ()
handle_close = c_close_handle . toHANDLE

handle_dev_type :: RawHandle a => a -> IO IODeviceType
handle_dev_type hwnd = do _type <- c_handle_type $ toHANDLE hwnd
                          return $ case _type of
                                     _ | _type == 3 -> Stream
                                       | _type == 5 -> RawDevice
                                       | otherwise  -> RegularFile

handle_is_seekable :: RawHandle a => a -> IO Bool
handle_is_seekable hwnd = do
  t <- handle_dev_type hwnd
  return (t == RegularFile || t == RawDevice)

handle_seek :: RawHandle a => a -> SeekMode -> Integer -> IO ()
handle_seek hwnd mode off =
  throwErrnoIf_ not "GHC.IO.Handle.handle_seek" $
      c_set_file_pointer (toHANDLE hwnd) (fromIntegral off) seektype
 where
    seektype :: DWORD
    seektype = case mode of
                   AbsoluteSeek -> #{const FILE_BEGIN}
                   RelativeSeek -> #{const FILE_CURRENT}
                   SeekFromEnd  -> #{const FILE_END}

handle_tell :: RawHandle a => a -> IO Integer
handle_tell hwnd =
   fromIntegral `fmap`
      (throwErrnoIfMinus1Retry "GHC.IO.Handle.handle_tell" $
          c_get_file_pointer (toHANDLE hwnd))

handle_set_size :: RawHandle a => a -> Integer -> IO ()
handle_set_size hwnd size =
  throwErrnoIf_ not "GHC.IO.Handle.handle_set_size" $
      c_set_file_size (toHANDLE hwnd) (fromIntegral size)

handle_get_size :: RawHandle a => a -> IO Integer
handle_get_size hwnd =
   fromIntegral `fmap`
      (throwErrnoIfMinus1Retry "GHC.IO.Handle.handle_set_size" $
          c_get_file_size (toHANDLE hwnd))

handle_set_echo :: RawHandle a => a -> Bool -> IO ()
handle_set_echo hwnd value =
  throwErrnoIf_ not "GHC.IO.Handle.handle_set_echo" $
      c_set_console_echo (toHANDLE hwnd) value

handle_get_echo :: RawHandle a => a -> IO Bool
handle_get_echo = c_get_console_echo . toHANDLE

handle_duplicate :: RawHandle a => a -> IO a
handle_duplicate hwnd = alloca $ \ptr -> do
  throwErrnoIf_ not "GHC.IO.Handle.handle_duplicate" $
      c_duplicate_handle (toHANDLE hwnd) ptr
  fromHANDLE <$> peek ptr

handle_set_buffering :: RawHandle a => a -> Bool -> IO ()
handle_set_buffering hwnd value =
  throwErrnoIf_ not "GHC.IO.Handle.handle_set_buffering" $
      c_set_console_buffering (toHANDLE hwnd) value

handle_console_seek :: RawHandle a => a -> SeekMode -> Integer -> IO ()
handle_console_seek hwnd mode off =
  throwErrnoIf_ not "GHC.IO.Handle.handle_console_seek" $
      c_set_console_pointer (toHANDLE hwnd) (fromIntegral off) seektype
 where
    seektype :: DWORD
    seektype = case mode of
                   AbsoluteSeek -> #{const FILE_BEGIN}
                   RelativeSeek -> #{const FILE_CURRENT}
                   SeekFromEnd  -> #{const FILE_END}

handle_console_tell :: RawHandle a => a -> IO Integer
handle_console_tell hwnd =
   fromIntegral `fmap`
      (throwErrnoIfMinus1Retry "GHC.IO.Handle.handle_console_tell" $
          c_get_console_pointer (toHANDLE hwnd))

handle_set_console_size :: RawHandle a => a -> Integer -> IO ()
handle_set_console_size hwnd size =
  throwErrnoIf_ not "GHC.IO.Handle.handle_set_console_size" $
      c_set_console_buffer_size (toHANDLE hwnd) (fromIntegral size)

handle_get_console_size :: RawHandle a => a -> IO Integer
handle_get_console_size hwnd =
   fromIntegral `fmap`
      (throwErrnoIfMinus1Retry "GHC.IO.Handle.handle_get_console_size" $
          c_get_console_buffer_size (toHANDLE hwnd))