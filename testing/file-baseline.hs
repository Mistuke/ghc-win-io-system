{-# LANGUAGE CPP, OverloadedStrings #-}

module Main where

import Prelude hiding (log)

import Data.Monoid
import GHC.Event.Windows
import Control.Concurrent
import Control.Monad        (forever, void)
import System.IO hiding (hGetContents)
import Data.ByteString
import qualified Data.ByteString.Char8 as B8
import Network hiding (accept)
import Network.Socket (accept)
import Network.Socket.ByteString

portNum :: PortNumber
portNum = 8080

client sock = do
  recvRequest ""
  withFile "iocp.cabal" ReadMode $ \h -> do
                fileCont <- hGetContents h
                sendAll sock (header <> fileCont)
                sClose sock
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
        (s, _) <- accept listenSock
        forkIO $ client s
        loop

main = do
  void $ getSystemManager
  sock <- listenOn (PortNumber portNum)
  runInUnboundThread $ acceptConnections sock
