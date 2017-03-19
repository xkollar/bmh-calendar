{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module MusicEvent where

import Data.String (String)
import Data.Typeable (Typeable)
import Text.Show (Show)

import Data.SafeCopy (base, deriveSafeCopy)
import Data.Time (LocalTime, UTCTime)
import Network.URI (URI)

import Hack ()

type Uid = String

data MusicEvent = MusicEvent
    { meCreated :: UTCTime
    , meSummary :: String
    , meStart :: LocalTime
    , meUid :: Uid -- Most likely just number, or place"/"number
    , meUrl :: URI
    , meGenres :: [String] -- Like ["klasicka-hudba", "pop"]
    , mePlace :: String -- Like "Sono centrum"
    , meAddress :: String -- Like "Veveri 123, Brno"
    , meDescription :: String
    }
  deriving (Show, Typeable)

deriveSafeCopy 0 'base ''MusicEvent
