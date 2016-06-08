{-# LANGUAGE CPP, OverloadedStrings #-}

module Main where

import Prelude hiding (log)

import Data.Monoid
import Control.Exception
import GHC.Event.Windows
import GHC.Event.Windows.Thread
import Control.Concurrent
import Control.Monad    (forever, void)
import System.IO
import System.IO.IOCP
import Network.Winsock
import Data.ByteString
import qualified Data.ByteString.Char8 as B8
import qualified Network.Socket as NS

portNum :: NS.PortNumber
portNum = 8080

client sock h = do
  recvRequest ""
  fileContents <- readFileIOCP h
  sendAll sock (header <> fileContents)
  close sock

 where
   header = "HTTP/1.0 200 OK\r\nConnection: Close\r\nContent-Length: 5\r\n\r\n"
   recvRequest r = do
    s <- recv sock 4096
    let t = B8.append r s
    if B8.null s || "\r\n\r\n" `B8.isInfixOf` t
      then return ()
      else recvRequest t

acceptConnections listenSock =
    bracket (openFileIOCP "iocp.cabal") closeFileIOCP loop
    where
      loop h = do
        sock <- accept listenSock
        forkIO $ client sock h
        loop h

main = do
  hSetBuffering stdout LineBuffering
  ensureIOManagerIsRunning
  sock <- socket NS.AF_INET NS.Stream NS.defaultProtocol
  addr <- NS.inet_addr "127.0.0.1"
  bind sock (NS.SockAddrInet portNum addr)
  listen sock
  runInUnboundThread $ acceptConnections sock
