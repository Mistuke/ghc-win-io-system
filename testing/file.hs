{-# LANGUAGE CPP, OverloadedStrings #-}

module Main where

import Prelude hiding (log)

import Data.Monoid
import GHC.Event.Windows
import GHC.Event.Windows.Thread
import Control.Concurrent
import Control.Monad        (forever, void)
import System.IO hiding (hGetContents)
import Network.Winsock
import Data.ByteString
import qualified Data.ByteString.Char8 as B8
import qualified Network.Socket as NS

portNum :: NS.PortNumber
portNum = 8080

client sock = do
  recvRequest ""
  withFile "iocp.cabal" ReadMode $ \h -> do
                fileContents <- hGetContents h
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

acceptConnections listenSock = loop
    where
      loop = do
        sock <- accept listenSock
        forkIO $ client sock
        loop

main = do
  ensureIOManagerIsRunning
  sock <- socket NS.AF_INET NS.Stream NS.defaultProtocol
  addr <- NS.inet_addr "127.0.0.1"
  bind sock (NS.SockAddrInet portNum addr)
  listen sock
  runInUnboundThread $ acceptConnections sock
