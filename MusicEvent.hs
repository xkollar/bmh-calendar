{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
module MusicEvent
    ( Uid
    , Genre
    , GenreKey
    , Place
    , PlaceKey
    -- * MusicEvent
    , MusicEvent(..)
    -- ** MusicEvent for various occasions
    , CreateMusicEvent
    , RetrievedMusicEvent
    , StoredMusicEvent
    )
  where

import Data.String (String)
import Data.Typeable (Typeable)
import Text.Show (Show)

import Data.SafeCopy (base, deriveSafeCopy)
import Data.Time (LocalTime, UTCTime)
import Network.URI (URI)

import Hack ()

type Uid = String

type Genre = String

type GenreKey = String

type Place = String

type PlaceKey = String

data MusicEvent genre place = MusicEvent
    { meCreated :: !UTCTime
    , meSummary :: !String
    , meStart :: !LocalTime
    , meUid :: !Uid
    , meUrl :: !URI
    , meGenres :: ![genre]
    , mePlace :: !place
    , meAddress :: !String -- Like "Veveri 123, Brno"
    , meDescription :: !String
    }
  deriving (Show, Typeable)

deriveSafeCopy 0 'base ''MusicEvent

type CreateMusicEvent = MusicEvent (GenreKey, Genre) (PlaceKey, Place)

type RetrievedMusicEvent = MusicEvent Genre Place

type StoredMusicEvent = MusicEvent GenreKey PlaceKey
