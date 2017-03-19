{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module EventStore where

import Data.Monoid ((<>))
import Data.String (fromString)

import Control.Monad.Catch (bracket)
import Control.Monad.Reader (asks)
import Control.Monad.State (modify)

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
import Data.Time (Day, LocalTime(..))

import FstOrd (FstOrd(FstOrd, _snd))
import MusicEvent
    ( CreateMusicEvent
    , RetrievedMusicEvent
    , Genre
    , GenreKey
    , MusicEvent(..)
    , Place
    , PlaceKey
    , StoredMusicEvent
    , Uid
    )
import TimeHelper (midnightOf)


data EventStore = EventStore
    { events :: !(Map Uid StoredMusicEvent)
    , genres :: !(Map GenreKey Genre)
    , places :: !(Map PlaceKey Place)
    , uidsAndDates :: !(Set (FstOrd LocalTime Uid))
    , uidsByGenre :: !(Map GenreKey (Set Uid))
    , uidsByPlace :: !(Map PlaceKey (Set Uid))
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

insertEvent :: CreateMusicEvent -> Update EventStore ()
insertEvent e@MusicEvent{..} = modify go
  where
    go st@EventStore{..} = st
        { events = Map.insert meUid ste events
        , genres = Map.fromList meGenres `Map.union` genres
        , places = uncurry Map.insert mePlace places
        , uidsAndDates = Set.insert (FstOrd meStart meUid) uidsAndDates
        , uidsByGenre = foldr addUidTo uidsByGenre gs
        , uidsByPlace = addUidTo placeId uidsByPlace
        }

    addUidTo g m = Map.insertWith (Set.union) g (Set.singleton meUid) m

    gs = map fst meGenres

    placeId = fst mePlace

    ste = e
        { meGenres = gs
        , mePlace = placeId
        }

between :: Ord a => Maybe a -> Maybe a -> Set a -> Set a
between from to = cut fst to . cut snd from
  where
    cut f = maybe id $ \ x -> f . Set.split x

getEvents
    :: Maybe Day
    -- ^ From
    -> Maybe Day
    -- ^ To
    -> [GenreKey]
    -- ^ Genres to include
    -> [GenreKey]
    -- ^ Genres to exclude
    -> Query EventStore (Map Uid RetrievedMusicEvent)
getEvents from to _ _ = asks go
  where
    go EventStore{..} = Map.map dereference $ limmitByDate events
      where
        limmitByDate = Map.filterWithKey (\ k _ -> Set.member k uids)

        uids = Set.map _snd $ between from' to' uidsAndDates

        dereference e@MusicEvent{..} = e
            { meGenres = map (findIn genres) meGenres
            , mePlace = findIn places mePlace
            }

        findIn m x = Map.findWithDefault (fromString "Chybka: " <> x) x m

    mk :: LocalTime -> FstOrd LocalTime Uid
    mk = flip FstOrd ""

    from' :: Maybe (FstOrd LocalTime Uid)
    from' = mk . midnightOf <$> from

    to' :: Maybe (FstOrd LocalTime Uid)
    to' = mk . midnightOf <$> to

isEvent :: Uid -> Query EventStore Bool
isEvent u = asks $ Map.member u . events

makeAcidic ''EventStore ['getEvents, 'insertEvent, 'isEvent]

withEventStore :: (AcidState EventStore -> IO ()) -> IO ()
withEventStore = bracket acquire release
  where
    acquire = openLocalStateFrom "events" emptyStore

    release st = createCheckpoint st >> closeAcidState st
