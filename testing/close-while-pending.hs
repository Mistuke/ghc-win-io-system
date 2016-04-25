{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-name-shadowing #-}
module Main where

import Prelude hiding (log)

import Control.Concurrent
import Control.Exception as E
import Control.Monad
import Data.ByteString.Char8 ()
import Network              (PortID(..))
import Network.Socket       (Family(..), SocketType(..), PortNumber
                            , SockAddr(..)
                            , defaultProtocol, inet_addr)
import System.IO
import Winsock
import qualified Network as N

portNum :: PortNumber
portNum = 1234

server = void $ forkIO $ do
    sock <- N.listenOn $ PortNumber portNum
    (h, host, port) <- N.accept sock
    hSetBuffering h NoBuffering
    let log msg = putStrLn $ "server: " ++ host
                  ++ ":" ++ show port ++ ": " ++ msg
    log "accepted connection"
    threadDelay 100000000
    log "closing connection"
    hClose h

main = do
    mapM_ (`hSetBuffering` LineBuffering) [stdout, stderr]
    server
    sock <- socket AF_INET Stream defaultProtocol
    addr <- inet_addr "127.0.0.1"
    connect sock $ SockAddrInet portNum addr

    let log msg = putStrLn $ "client: " ++ msg

    tid <- forkIO $ do
        log "recv (1)"
        (recv sock 100 >>= print) `E.catch` \ex -> do
            log $ "recv failed: " ++ show (ex :: SomeException)

        log "recv (2)"
        (recv sock 100 >>= print) `E.catch` \ex -> do
            log $ "recv failed: " ++ show (ex :: SomeException)
            log "closing socket from recv thread"
            close sock

    threadDelay 1000000
    log "killing recv"
    killThread tid

    threadDelay 1000000
    log "closing socket"
    close sock
    log "socket closed"

    threadDelay 10000000
