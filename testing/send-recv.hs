-- Make sure CancelIo doesn't cancel unrelated operations.

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-name-shadowing #-}
module Main where

import Prelude hiding (log)

import Control.Concurrent
import Control.Monad
import Data.IORef
import Network              (PortID(..))
import Network.Socket       (Family(..), SocketType(..), PortNumber
                            , SockAddr(..), defaultProtocol, inet_addr)
import System.IO
import System.Timeout       (timeout)
import Winsock
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Network as N
-- import qualified Network.Socket as NS
-- import qualified Network.Socket.ByteString as NSB

portNum :: PortNumber
portNum = 1234

server :: MVar () -> IO ()
server mv = void $ forkIO $ do
    sock <- N.listenOn $ PortNumber portNum
    (h, host, port) <- N.accept sock
    hSetBuffering h NoBuffering
    let log msg = putStrLn $ "server: " ++ host
                  ++ ":" ++ show port ++ ": " ++ msg
    log "accepted connection"

    takeMVar mv

    log "emptying send buffer"
    _ <- forkIO $ forever $ B.hGetSome h 4096
    threadDelay 1000000

    log "sending bye"
    B.hPut h "Bye"
    threadDelay 1000000

    log "closing"
    hClose h
    log "closed"

main = do
    mapM_ (`hSetBuffering` LineBuffering) [stdout, stderr]
    readyForServerToSend <- newEmptyMVar
    server readyForServerToSend
    sock <- socket AF_INET Stream defaultProtocol
    addr <- inet_addr "127.0.0.1"
    connect sock $ SockAddrInet portNum addr

    readyToRecv <- newEmptyMVar

    _ <- forkIO $ do
        let log msg = putStrLn $ "client recv: " ++ msg
        takeMVar readyToRecv
        log "waiting for response from server"
        replicateM_ 3 $ do
            msg <- recv sock 100
            log $ "received " ++ show msg

    let log msg = putStrLn $ "client send: " ++ msg
    let chunk = B8.pack ['\0' .. '\255']

    log "filling up the send buffer"
    sent_count <- newIORef (0 :: Int)
    Nothing <- timeout 2000000 $
        forM_ [1..1000] $ \i -> do
            256 <- send sock chunk
            writeIORef sent_count i
            return ()
    do n <- readIORef sent_count
       log $ "done (sent " ++ show n ++ "/1000 chunks)"

    putMVar readyToRecv ()

    log "performing blocking send"
    Nothing <- timeout 2000000 $
        send sock chunk
    log "timed out the send"

    putMVar readyForServerToSend ()

    threadDelay 100000000
