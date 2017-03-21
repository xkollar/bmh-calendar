{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
module Effects.HttpClient where

import Control.Monad ((>>=), return)
import Data.Either
import Data.Function ((.))
import Data.String (String)
import System.IO (IO)

import Control.Monad.Freer (Eff, Member, send)
import Control.Monad.Freer.Internal
    (Eff(E, Val), decomp, qApp, tsingleton)
import Data.ByteString.Lazy (ByteString)


data HttpClient s where
  HttpGet :: String -> HttpClient ByteString

httpGet :: Member HttpClient r => String -> Eff r ByteString
httpGet = send . HttpGet

runHttpClient
    :: Member IO effs
    => (String -> Eff effs ByteString) -> Eff (HttpClient ': effs) a -> Eff effs a
runHttpClient _ (Val x) = return x
runHttpClient f (E u q) = case decomp u of
    Right (HttpGet s) -> f s >>= runHttpClient f . qApp q
    Left u'              -> E u' (tsingleton (runHttpClient f . qApp q))
