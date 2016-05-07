-- Compile with -threaded, or you'll get a pattern match failure.
-- This library assumes a threaded runtime system.

{-# OPTIONS -fno-warn-missing-signatures #-}
import Network.Socket hiding (socket, connect)
import qualified Network.Winsock as Winsock

import Control.Concurrent   (threadDelay)
import Control.Exception    (finally)
import Control.Monad        (forever)
import System.IO
import System.Timeout       (timeout)

googleIP = "74.125.140.138"

main = do
    mapM_ (`hSetBuffering` LineBuffering) [stdout, stderr]
    forever $ timeout 1000000 $ do
        putStrLn "Connecting"
        sock <- Winsock.socket AF_INET Stream defaultProtocol
        addr <- inet_addr googleIP
        Winsock.connect sock (SockAddrInet 1234 addr)
                   `finally` Winsock.close sock

        -- Avoid making successful connections repeatedly.
        threadDelay 10000000
