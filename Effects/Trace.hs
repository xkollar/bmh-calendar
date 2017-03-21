{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Effects.Trace where

import Control.Monad ((>>=), return)
import Data.Either (Either(Left, Right))
import Data.Function (($), (.), const)
import Data.String (String)
import System.IO (IO)
import Text.Printf (printf)
import Text.Show (show)

import Data.Time (getCurrentTime)

import Control.Monad.Freer.Internal
    (Eff(E, Val), Member, decomp, qApp, send, tsingleton)


data Trace s a where
    Trace :: s -> Trace s ()

type StringTrace = Effects.Trace.Trace String

-- | Printing a string in a trace.
trace :: Member (Trace s) effs => s -> Eff effs ()
trace = send . Trace

runTrace :: (s -> Eff effs ()) -> Eff (Trace s ': effs) a -> Eff effs a
runTrace _ (Val x) = return x
runTrace f (E u q) = case decomp u of
    Right (Trace s) -> f s >>= runTrace f . qApp q
    Left u' -> E u' (tsingleton (runTrace f . qApp q))

runTraceIO :: Member IO effs => Eff (Trace String ': effs) a -> Eff effs a
runTraceIO = runTrace f
  where
    f s = send $ do
        t <- getCurrentTime
        printf "[%s]: %s\n" (show t) s

runTraceSilent :: forall proxy effs s a . proxy s -> Eff (Trace s ': effs) a -> Eff effs a
runTraceSilent p = runTrace (f p)
  where
    f :: proxy s -> s -> Eff effs ()
    f _ = const (return ())
