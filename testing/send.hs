{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-name-shadowing #-}
module Main where

import Prelude hiding (log)

import Control.Concurrent
import Control.Monad        (forever, void)
import System.IO
import Network.Winsock
import qualified Data.ByteString.Char8 as B8
import qualified Network.Socket as NS

portNum :: NS.PortNumber
portNum = 8080

server = do
    addr <- NS.inet_addr "127.0.0.1"
    sock <- socket NS.AF_INET NS.Stream NS.defaultProtocol
    bind sock (NS.SockAddrInet portNum addr)
    listen sock
    let log msg = putStrLn $ "server: " ++ msg
    void $ forkIO $ forever $ do
        log "started listening"
        s <- accept sock
        log "accepted connection"
        msg <- recv s 10
        log $ "received " ++ show msg
        msg <- recv s 10
        log $ "received " ++ show msg
        msg <- recv s 10
        log $ "received " ++ show msg
        log "closing connection"
        close s
        log "done"

main = do
    mapM_ (`hSetBuffering` LineBuffering) [stdout, stderr]
    server
    let log msg = putStrLn $ "client: " ++ msg
    sock <- socket NS.AF_INET NS.Stream NS.defaultProtocol
    addr <- NS.inet_addr "127.0.0.1"
    connect sock $ NS.SockAddrInet portNum addr
    log "connected"
    threadDelay 1000000
    log "sending message"
    n <- send sock $ B8.pack "Hello"
    log $ "sent " ++ show n ++ " bytes"
    threadDelay 1000000
    log "closing connection"
    close sock
    log "done"

    threadDelay 100000000
