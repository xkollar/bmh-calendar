{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
module Tools.Delayed (delayed) where

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
import System.IO (IO)
import System.IO.Unsafe (unsafePerformIO)
import Text.Show (show)

import Control.Monad.Freer (Eff, Members, send)
import Data.Time
    (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)

import Effects.Trace (StringTrace, trace)


nextAllowed :: MVar UTCTime
nextAllowed = unsafePerformIO $ getCurrentTime >>= newMVar

microSecondsDelta :: UTCTime -> UTCTime -> Int
microSecondsDelta t1 t2 = round . toRational $ diffUTCTime t1 t2 * 1000000

delayed :: Members '[StringTrace, IO] r => NominalDiffTime -> Eff r a -> Eff r a
delayed dt a = do
    tTill <- send $ takeMVar nextAllowed
    tNow <- send $ getCurrentTime
    let waitTime = max 0 $ microSecondsDelta tTill tNow
    trace $ "Waiting " <> show waitTime
    send . threadDelay $ waitTime
    res <- a >>= send . evaluate
    send $ getCurrentTime >>= putMVar nextAllowed . addUTCTime dt
    pure res
