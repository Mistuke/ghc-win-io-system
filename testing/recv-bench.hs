{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-name-shadowing #-}
module Main where

import Prelude hiding (log)

import Control.Concurrent
import Control.Monad        (forever, void)
import Criterion.Main
import Network              (PortID(..))
import Network.Socket       (Family(..), SocketType(..), PortNumber
                            , SockAddr(..), defaultProtocol, inet_addr)
import System.IO
import Network.Winsock
import GHC.Event.Windows (getSystemManager)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Network as N
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

portNum :: PortNumber
portNum = 1234

server = do
    sock <- N.listenOn $ PortNumber portNum
    void $ forkIO $ forever $ do
        (h, host, port) <- N.accept sock
        forkIO $ do
            hSetBuffering h NoBuffering
            let log msg = putStrLn $ "server: " ++ host
                          ++ ":" ++ show port ++ ": " ++ msg
            log "accepted connection"

            let chunk = B.concat $ replicate 16 $ B8.pack ['\0' .. '\255']
            forever $ B.hPut h chunk

main = do
    mapM_ (`hSetBuffering` LineBuffering) [stdout, stderr]
    server
    addr <- inet_addr "127.0.0.1"

    sock <- socket AF_INET Stream defaultProtocol
    connect sock $ SockAddrInet portNum addr

    nsbSock <- NS.socket AF_INET Stream defaultProtocol
    NS.connect nsbSock $ SockAddrInet portNum addr

    void $ getSystemManager
    threadDelay 1000000

    runInUnboundThread $ defaultMain
        [ bench "Winsock.recv" $
            whnfIO $ recv sock 1
        , bench "Network.Socket.ByteString.recv" $
            whnfIO $ NSB.recv nsbSock 1
        ]
