{-# LANGUAGE NoImplicitPrelude #-}
module Delayed (delayed) where

import Prelude ((*), round, toRational)

import Control.Applicative (pure)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Exception.Base (evaluate)
import Control.Monad ((>>=))
import Data.Function (($), (.))
import Data.Int (Int)
import Data.Monoid ((<>))
import Data.Ord (max)
import System.IO (IO, putStrLn)
import System.IO.Unsafe (unsafePerformIO)
import Text.Show (show)

import Data.Time
    (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)

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
