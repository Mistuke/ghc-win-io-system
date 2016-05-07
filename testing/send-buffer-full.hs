{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-name-shadowing #-}
module Main where

import Prelude hiding (log)

import Control.Concurrent
import Control.Monad        (forever, forM_, void)
import Network              (PortID(..))
import Network.Socket       (Family(..), SocketType(..), PortNumber
                            , SockAddr(..), defaultProtocol, inet_addr)
import System.IO
import System.Timeout       (timeout)
import Network.Winsock
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Network as N

portNum :: PortNumber
portNum = 1234

server = do
    sock <- N.listenOn $ PortNumber portNum
    void $ forkIO $ forever $ do
        (h, host, port) <- N.accept sock
        let log msg = putStrLn $ "server: " ++ host
                      ++ ":" ++ show port ++ ": " ++ msg
        log "accepted connection"
        msg <- B.hGetSome h 10
        log $ "received " ++ show msg
        msg <- B.hGetSome h 10
        log $ "received " ++ show msg
        msg <- B.hGetSome h 10
        log $ "received " ++ show msg

        threadDelay 15000000
        log "closing connection"
        hClose h
        log "done"

main = do
    mapM_ (`hSetBuffering` LineBuffering) [stdout, stderr]
    server
    let log msg = putStrLn $ "client: " ++ msg
    sock <- socket AF_INET Stream defaultProtocol
    addr <- inet_addr "127.0.0.1"
    connect sock $ SockAddrInet portNum addr
    log "connected"
    threadDelay 1000000
    log "sending message"
    n <- send sock $ B8.pack "Hello"
    log $ "sent " ++ show n ++ " bytes"
    threadDelay 1000000

    let chunk = B8.pack ['\0' .. '\255']
    log "sending lots of data"
    Nothing <- timeout 2000000 $
        forM_ [1..1000] $ \i -> do
            log $ "chunk " ++ show (i :: Int)
            256 <- send sock chunk
            return ()
    log "timed that out"

    log "more sends that should time out"
    forM_ [1..100] $ \i -> do
        Nothing <- timeout 500000 $ do
            log $ "chunk " ++ show (i :: Int)
            256 <- send sock chunk
            return ()
        return ()
    log "done with that"

    threadDelay 1000000
    log "closing connection"
    close sock
    log "done"

    threadDelay 100000000
