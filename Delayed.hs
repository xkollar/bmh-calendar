module Delayed (delayed) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception.Base (evaluate)
import Data.Monoid ((<>))
import System.IO.Unsafe

import Data.Time

nextAllowed :: MVar UTCTime
nextAllowed = unsafePerformIO $ getCurrentTime >>= newMVar

microSecondsDelta :: UTCTime -> UTCTime -> Int
microSecondsDelta t1 t2 = round . toRational $ diffUTCTime t1 t2 * 1000000

delayed :: NominalDiffTime -> IO a -> IO a
delayed dt a = do
    tTill <- takeMVar nextAllowed
    tNow <- getCurrentTime
    let waitTime = max 0 $ microSecondsDelta tTill tNow
    putStrLn $ "Waiting " <> show waitTime
    threadDelay $ waitTime
    res <- a >>= evaluate
    getCurrentTime >>= putMVar nextAllowed . addUTCTime dt
    pure res
