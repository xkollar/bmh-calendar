{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module EventStore where

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

import FstOrd (FstOrd(FstOrd))
import MusicEvent (Genre, MusicEvent(..), Uid)


data EventStore = EventStore
    { events :: !(Map Uid MusicEvent)
    , genres :: !(Map Genre String)
    , places :: !(Map Genre String)
    , uidsAndDates :: !(Set (FstOrd LocalTime Uid))
    , uidsByGenre :: !(Map Genre (Set Uid))
    , uidsByPlace :: !(Map Genre (Set Uid))
    }

deriveSafeCopy 0 'base ''EventStore

emptyStore :: EventStore
emptyStore = EventStore
    { events = Map.empty
    , genres = Map.empty
    , places = Map.empty
    , uidsAndDates = Set.empty
    , uidsByGenre = Map.empty
    , uidsByPlace = Map.empty
    }

insertEvent :: MusicEvent -> Update EventStore ()
insertEvent e@MusicEvent{..} = modify go
  where
    go st@EventStore{..} = st
        { events = Map.insert meUid e events
        , uidsAndDates = Set.insert (FstOrd meStart meUid) uidsAndDates
        }

allEvents :: Query EventStore (Map Uid MusicEvent)
allEvents = asks $ events

isEvent :: Uid -> Query EventStore Bool
isEvent u = asks $ Map.member u . events

makeAcidic ''EventStore ['allEvents, 'insertEvent, 'isEvent]

withEventStore :: (AcidState EventStore -> IO ()) -> IO ()
withEventStore = bracket acquire release
  where
    acquire = openLocalStateFrom "events" emptyStore

    release st = createCheckpoint st >> closeAcidState st
