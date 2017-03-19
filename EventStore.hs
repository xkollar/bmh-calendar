{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module EventStore where

import Prelude

import Control.Monad.Catch (bracket)
import Control.Monad.State (modify)
import Control.Monad.Reader (asks)

import Data.Acid
    ( AcidState
    , Query
    , Update
    , closeAcidState
    , createCheckpoint
    , makeAcidic
    , openLocalStateFrom
    )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time (LocalTime)

import FstOrd
import MusicEvent


data EventStore = EventStore
    { events :: !(Map Uid MusicEvent)
    , uidsAndDates :: !(Set (FstOrd LocalTime Uid))
    , uidsByGenre :: !(Map String (Set Uid))
    }

deriveSafeCopy 0 'base ''EventStore

emptyStore :: EventStore
emptyStore = EventStore
    { events = Map.empty
    , uidsAndDates = Set.empty
    , uidsByGenre = Map.empty
    }

insertEvent :: MusicEvent -> Update EventStore ()
insertEvent e@MusicEvent{..} = modify go
  where
    go st@EventStore{..} = st
        { events = Map.insert meUid e events
        , uidsAndDates = Set.insert (FstOrd meStart meUid) uidsAndDates
        }

getEvents :: Query EventStore [MusicEvent]
getEvents = asks $ Map.elems . events

isEvent :: Uid -> Query EventStore Bool
isEvent u = asks $ Map.member u . events

makeAcidic ''EventStore ['getEvents, 'insertEvent, 'isEvent]

withEventStore :: (AcidState EventStore -> IO ()) -> IO ()
withEventStore = bracket acquire release
  where
    acquire = openLocalStateFrom "events" emptyStore

    release st = createCheckpoint st >> closeAcidState st
