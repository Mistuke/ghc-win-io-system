import GHC.Event.Windows.FFI

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import System.IO
import System.IO.Error

-- From the ansi-terminal package.  Used to remove blank lines introduced by
-- user hitting enter to get the next time.
import System.Console.ANSI (cursorUpLine)

handleEOF :: IO () -> IO ()
handleEOF = handleJust (guard . isEOFError) (\_ -> return ())

main :: IO ()
main = do
    mapM_ (`hSetBuffering` LineBuffering) [stdout, stderr]
    freq  <- queryPerformanceFrequency

    when (isNothing freq) $
        putStrLn "QueryPerformanceCounter not available"

    let printTimes = do
            t64 <- getTickCount64
            qpc <- case freq of
                Nothing -> return Nothing
                Just f  -> (\c -> Just (c, f)) <$> queryPerformanceCounter
            putStrLn $ formatTimes t64 qpc

        formatTimes t64 qpc =
            concat $ intersperse "\t"
            [ show t64 ++ "ms"
            , case qpc of
                  Nothing    -> "n/a"
                  Just (n,d) ->
                      concat
                      [ show (divTime n d), "s"
                      , "\t(", show n, "/", show d, ")"
                      ]
            ]

        divTime n d = fromIntegral n / fromIntegral d :: Double

    putStrLn ""
    putStrLn "GetTickCount64\tQueryPerformanceCounter"

    handleEOF $ forever $ do
        printTimes
        _ <- getLine
        cursorUpLine 1
