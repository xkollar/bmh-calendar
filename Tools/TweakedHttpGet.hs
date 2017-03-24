{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tools.TweakedHttpGet (get) where

import Control.Applicative (pure)
import Data.Function (($))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>))
import Data.String (String)
import System.IO (IO)

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Freer (Eff, Members, send)
import qualified Data.ByteString.Lazy as BSL
import Network.Wreq
    (Options, checkStatus, cookies, defaults, getWith, headers, responseBody)

import Effects.Trace (StringTrace, trace)

myOpts :: Options
myOpts = defaults
    & cookies .~ Nothing
    & checkStatus .~ noCheck
    & headers .~ [("User-Agent", "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:40.0) Gecko/20100101 Firefox/40.1")]
  where
    noCheck = Just $ \ _ _ _ -> Nothing

get :: Members '[StringTrace, IO] r => String -> Eff r BSL.ByteString
get url = do
    trace $ "Getting " <> url
    res <- send $ getWith myOpts url
    pure $ res ^. responseBody
