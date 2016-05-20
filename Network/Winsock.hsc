{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Network.Winsock (
    Socket(..),
    SOCKET,
    socket,
    bind,
    listen,
    accept,
    connect,
    shutdown,
    close,
    recvBuf,
    sendBuf,

    recv,
    send,
    sendAll
) where

#include <winsock2.h>
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

import GHC.Event.Windows               (Overlapped(..))
import qualified GHC.Event.Windows.FFI as FFI
import qualified GHC.Event.Windows     as Mgr

import Control.Monad            (void, when)
import Data.ByteString          (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Internal (createAndTrim)
import Data.ByteString.Unsafe   (unsafeUseAsCStringLen)
import Data.IORef
import Data.Word
import Foreign.C
import Foreign.Ptr
import Network.Socket.Internal  (withSockAddr)
import System.IO.Unsafe         (unsafePerformIO)
import System.Win32.Types

import qualified Network.Socket     as NS
import qualified System.Win32.Types as Win32

newtype Socket = Socket { sockFd :: SOCKET }
    deriving Eq

-- Note: Functions that take a 'Socket' expect Winsock to already be initialized.

socket :: NS.Family -> NS.SocketType -> NS.ProtocolNumber -> IO Socket
socket family stype protocol = do
    initWinsock
    fd <- fromIntegral . NS.fdSocket <$> NS.socket family stype protocol
    mgr <- getManager
    Mgr.associateHandle mgr (castSOCKETToHANDLE fd)
    return (Socket fd)

getManager :: IO Mgr.Manager
getManager = Mgr.getSystemManager >>= maybe (fail "requires threaded RTS") return

withOverlapped :: SOCKET -> Word64
               -> (Overlapped -> IO ())
               -> Mgr.CompletionCallback a
               -> IO a
withOverlapped h offset startCB completionCB = do
    Mgr.withOverlapped (castSOCKETToHANDLE h) offset startCB completionCB

listen :: Socket -> IO ()
listen (Socket sock) = do
  Win32.failIf_ (/= 0) "listen" $ c_listen sock (#const SOMAXCONN)

bind :: Socket -> NS.SockAddr -> IO ()
bind (Socket sock) addr = do
  withSockAddr addr $ \addr_ptr addr_len ->
      Win32.failIf_ (/= 0) "bind" $ c_bind sock addr_ptr (fromIntegral addr_len)

accept :: Socket -> IO Socket
accept (Socket listenSock) = do
  winsock <- getWinsock
  acceptSock <- socket NS.AF_INET NS.Stream NS.defaultProtocol
  withOverlapped listenSock 0 (startCB winsock acceptSock)
                     (completionCB acceptSock)
  return acceptSock
    where
      startCB winsock (Socket acceptSock) overlapped =
          Win32.failIfFalse_ "accept" $
          c_winsock_accept winsock listenSock acceptSock overlapped

      completionCB (Socket acceptSock) err _numBytes
          | err == 0  = c_winsock_accept_on_completion listenSock acceptSock
          | otherwise = FFI.throwWinErr "accept" err

connect :: Socket -> NS.SockAddr -> IO ()
connect (Socket sock) addr = do
    winsock <- getWinsock
    withOverlapped sock 0 (startCB winsock) completionCB
  where
    startCB winsock overlapped =
        withSockAddr addr $ \addr_ptr addr_len ->
        Win32.failIfFalse_ "connect" $
        c_winsock_connect winsock sock
                          addr_ptr (fromIntegral addr_len)
                          overlapped

    completionCB err _numBytes
        | err == 0  = return ()
        | otherwise = FFI.throwWinErr "connect" err

shutdown :: Socket -> NS.ShutdownCmd -> IO ()
shutdown sock how =
    Win32.failIf_ (/= 0) "shutdown" $
    c_shutdown (sockFd sock) (sdownCmdToInt how)

sdownCmdToInt :: NS.ShutdownCmd -> CInt
sdownCmdToInt NS.ShutdownReceive = #const SD_RECEIVE
sdownCmdToInt NS.ShutdownSend    = #const SD_SEND
sdownCmdToInt NS.ShutdownBoth    = #const SD_BOTH

close :: Socket -> IO ()
close = Win32.failIf_ (/= 0) "close" . c_closesocket . sockFd

recvBuf :: Socket -> Ptr a -> Int -> IO Int
recvBuf (Socket sock) buf len =
    withOverlapped sock 0 startCB completionCB
  where
    startCB ol =
        Win32.failIfFalse_ "recv" $
        c_winsock_recv sock (castPtr buf) (fromIntegral len) ol

    completionCB err numBytes
        | err == 0  = return (fromIntegral numBytes)
        | otherwise = FFI.throwWinErr "recv" err

sendBuf :: Socket -> Ptr a -> Int -> IO Int
sendBuf (Socket sock) buf len =
    withOverlapped sock 0 startCB completionCB
  where
    startCB ol =
        Win32.failIfFalse_ "send" $
        c_winsock_send sock (castPtr buf) (fromIntegral len) ol

    completionCB err numBytes
        | err == 0  = return (fromIntegral numBytes)
        | otherwise = FFI.throwWinErr "send" err

recv :: Socket -> Int -> IO ByteString
recv sock len =
    createAndTrim len $ \buf ->
    recvBuf sock buf len

sendAll :: Socket -> ByteString  -> IO ()
sendAll sock bs = do
    sent <- send sock bs
    when (sent < B.length bs) $ sendAll sock (B.drop sent bs)

send :: Socket -> ByteString -> IO Int
send sock bs =
    unsafeUseAsCStringLen bs $ \(buf, len) ->
    sendBuf sock buf len

newtype Winsock = Winsock (Ptr ())

getWinsock :: IO Winsock
getWinsock = readIORef winsockRef

initWinsock :: IO ()
initWinsock = void getWinsock

winsockRef :: IORef Winsock
winsockRef = unsafePerformIO (c_winsock_init >>= newIORef)
{-# NOINLINE winsockRef #-}

type SOCKET = #type SOCKET

castSOCKETToHANDLE :: SOCKET -> HANDLE
castSOCKETToHANDLE = wordPtrToPtr . fromIntegral

foreign import ccall unsafe
    c_winsock_init :: IO Winsock

foreign import WINDOWS_CCONV unsafe "winsock2.h bind"
  c_bind :: SOCKET -> Ptr NS.SockAddr -> CInt -> IO CInt

foreign import WINDOWS_CCONV unsafe "winsock2.h listen"
  c_listen :: SOCKET -> CInt -> IO CInt

foreign import ccall unsafe
    c_winsock_accept :: Winsock -> SOCKET -> SOCKET -> Overlapped -> IO BOOL

foreign import ccall unsafe
    c_winsock_accept_on_completion :: SOCKET -> SOCKET -> IO ()

foreign import ccall unsafe
    c_winsock_connect :: Winsock -> SOCKET -> Ptr NS.SockAddr -> CInt -> Overlapped -> IO BOOL

foreign import WINDOWS_CCONV safe "winsock2.h shutdown"
    c_shutdown :: SOCKET -> CInt -> IO CInt

foreign import WINDOWS_CCONV safe "winsock2.h closesocket"
    c_closesocket :: SOCKET -> IO CInt

foreign import ccall unsafe
    c_winsock_recv :: SOCKET -> Ptr CChar -> #{type u_long} -> Overlapped -> IO BOOL

foreign import ccall unsafe
    c_winsock_send :: SOCKET -> Ptr CChar -> #{type u_long} -> Overlapped -> IO BOOL
