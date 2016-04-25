module Main where

import IOCP.Clock

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Maybe
import System.IO
import System.IO.Error

-- From the ansi-terminal package.  Used to remove blank lines introduced by
-- user hitting enter to get the next time.
import System.Console.ANSI (cursorUpLine)

handleEOF :: IO () -> IO ()
handleEOF = handleJust (guard . isEOFError) (\_ -> return ())

maybeGetTime :: Maybe Clock -> IO (Maybe Seconds)
maybeGetTime = maybe (return Nothing) (fmap Just . getTime)

formatTimes :: Seconds -> Maybe Seconds -> Maybe Seconds -> String
formatTimes gtc gtc64 qpc =
    concat $ map (pad 16)
    [ pad 16 $ show gtc
    , pad 16 $ maybe "n/a" show gtc64
    , pad 25 $ maybe "n/a" show qpc
    , maybe "n/a" show $ liftA2 (-) qpc (gtc64 <|> Just gtc)
    ]

pad :: Int -> String -> String
pad n str = str ++ replicate (n - length str) ' '

main :: IO ()
main = do
    mapM_ (`hSetBuffering` LineBuffering) [stdout, stderr]
    gtc   <- getTickCount
    gtc64 <- getTickCount64
    qpc   <- queryPerformanceCounter

    when (isNothing gtc64) $
        putStrLn "GetTickCount64 not available"
    when (isNothing qpc) $
        putStrLn "QueryPerformanceCounter not available"

    let printTimes = liftM3 formatTimes (getTime gtc)
                                        (maybeGetTime gtc64)
                                        (maybeGetTime qpc)
                 >>= putStrLn

    putStrLn ""
    putStrLn "GetTickCount    GetTickCount64  QueryPerformanceCounter  QPC-GTC"

    handleEOF $ forever $ do
        printTimes
        _ <- getLine
        cursorUpLine 1
