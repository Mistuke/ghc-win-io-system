{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-name-shadowing #-}
module Main where

import Prelude hiding (log)

import Control.Concurrent
import Control.Monad
import Data.ByteString.Char8 ()
import Network              (PortID(..))
import Network.Socket       (Family(..), SocketType(..), PortNumber
                            , SockAddr(..), defaultProtocol, inet_addr)
import System.IO
import Winsock
import qualified Data.ByteString as B
import qualified Network as N

portNum :: PortNumber
portNum = 1234

server = void $ forkIO $ do
    sock <- N.listenOn $ PortNumber portNum
    (h, host, port) <- N.accept sock
    hSetBuffering h NoBuffering
    let log msg = putStrLn $ "server: " ++ host
                  ++ ":" ++ show port ++ ": " ++ msg

    log "accepted connection; waiting for input"

    msg <- B.hGetSome h 100
    log $ "received " ++ show msg

    log "closing connection"
    B.hPut h "Bye"
    hClose h

main = do
    mapM_ (`hSetBuffering` LineBuffering) [stdout, stderr]
    server
    sock <- socket AF_INET Stream defaultProtocol
    addr <- inet_addr "127.0.0.1"
    connect sock $ SockAddrInet portNum addr

    let log msg = putStrLn $ "client: " ++ msg

    log "send 1"
    3 <- send sock "Bye"

    log "recv 1"
    recv sock 100 >>= print

    log "recv 2"
    recv sock 100 >>= print

    log "recv 3"
    recv sock 100 >>= print

    threadDelay 1000000
    putStrLn "Done."
