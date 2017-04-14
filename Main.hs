{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Prelude hiding (log)

import Control.Arrow
import Control.Monad
import Data.Char (isDigit)
import Data.List
import Data.Maybe
import Data.Monoid ((<>))
import Data.String (fromString)

import Control.Monad.Freer
import Control.Monad.Freer.Reader
-- import Data.Acid hiding (query, update)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Default (def)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text.Lazy as Text.Lazy (unpack)
import qualified Data.Text.Lazy.Encoding as Text.Lazy (encodeUtf8, decodeUtf8)
import Data.Time
    ( addDays
    , fromGregorian
    , getCurrentTime
    , toGregorian
    , utctDay
    )
import Data.Tree.NTree.TypeDefs
import Network.URI (parseURI)
import Text.HandsomeSoup
import Text.ICalendar
import Text.XML.HXT.Core hiding (multi, trace)

import Tools.Delayed (delayed)
import Tools.Cached (cached)
import Tools.TweakedHttpGet (get)
import EventStore
    ( EventStore
    , GetEvents(GetEvents)
    , InsertEvent(InsertEvent)
    , IsEvent(IsEvent)
    , withEventStore
    )
import MusicEvent
    ( CreateMusicEvent
    , Genre
    , GenreKey
    , MusicEvent(..)
    , RetrievedMusicEvent
    , Uid
    )
import TimeHelper (readDate)
import Effects.Acid (AcidAccess, query, update)
import Effects.HttpClient (HttpClient, httpGet, runHttpClient)
import Effects.Trace (StringTrace, runTraceIO, trace)


log :: Member StringTrace r => String -> Eff r ()
log = trace

getDoc :: Member HttpClient r => String -> Eff r (IOSArrow b (NTree XNode))
getDoc url = parseHtml' <$> httpGet url
  where
    parseHtml' = parseHtml . filter ('\r'/=) . Text.Lazy.unpack . Text.Lazy.decodeUtf8

url2uid :: String -> Uid
url2uid = reverse . takeWhile isDigit . reverse

fetchEvent :: Members '[HttpClient, IO] r => String -> Eff r CreateMusicEvent
fetchEvent url = do
    doc <- getDoc url
    let extractWith e f = fmap e . runX $ doc >>> f
        extract' = extractWith (fromMaybe "Neuvedeno" . listToMaybe)
        extract = extractWith id
    send $ mkMusicEvent
        <$> getCurrentTime
        <*> (extract' $ css "h1" //> getText)
        <*> ((extract' $ css ".eta" /> getText) >>= readDate)
        <*> parsedUrl
        <*> (extract
            $ css ".category div"
            >>> ((css ".header" >>> deep getText)
                &&& (css ".list a" >>> (getAttrValue "href" &&& deep getText))))
        <*> (extract' $ css ".location" //> getText)
        <*> (extractWith (intercalate " ") $ css ".content" //> getText)
  where
    parsedUrl = maybe (fail $ "bad uri: " <> show url) pure $ parseURI url

    uid = url2uid url

    mkMusicEvent c smr strt u multi addr dsc = MusicEvent
        { meCreated = c
        , meSummary = smr
        , meStart = strt
        , meUid = uid
        , meUrl = u
        , meGenres = mkGenres multi
        , mePlace = mkPlace multi
        , meAddress = addr
        , meDescription = dsc
        }

    mkPlace = maybe ("unspecified", "Neuvedeno") (first cutGenre) . lookup "Lokace"

    mkGenres :: [(String,(String,String))] -> [(GenreKey, Genre)]
    mkGenres = map (first cutGenre . snd) . filter (("Žánr"==) . fst)

    cutGenre = reverse . takeWhile ('/'/=) . reverse

addEvent
    :: Members '[AcidAccess EventStore, HttpClient, StringTrace, IO] r
    => String -> Eff r ()
addEvent url = do
    b <- query . IsEvent $ url2uid url
    unless b $ do
        log $ "Fetching & storing event " <> url
        fetchEvent url >>= update . InsertEvent

musicEvent2VEvent :: RetrievedMusicEvent -> VEvent
musicEvent2VEvent MusicEvent{..} = VEvent
    { veDTStamp = DTStamp meCreated def
    , veUID = UID (fromString meUid) def
    , veClass = def
    , veDTStart = Just $ DTStartDateTime (FloatingDateTime meStart) def
    , veCreated = Nothing
    , veDescription = Just Description
        { descriptionValue = fromString
            $ "Žánr: " <> intercalate ", " meGenres <> "\n"
            <> "Lokace: " <> mePlace <> "\n\n"
            <> meDescription
        , descriptionAltRep = Nothing
        , descriptionLanguage = Just $ Language "cs"
        , descriptionOther = def
        }
    , veGeo = Nothing
    , veLastMod = Nothing
    , veLocation = Just Location
        { locationValue = fromString $ meAddress
        , locationAltRep = Nothing
        , locationLanguage = Nothing
        , locationOther = def
        }
    , veOrganizer = Nothing
    , vePriority = def
    , veSeq = def
    , veStatus = Nothing
    , veSummary = Just Summary
        { summaryValue = fromString meSummary
        , summaryAltRep = Nothing
        , summaryLanguage = Just $ Language "cs"
        , summaryOther = def
        }
    , veTransp = def
    , veUrl = Just $ URL meUrl def
    , veRecurId = Nothing
    , veRRule = def
    , veDTEndDuration = Nothing
    , veAttach = def
    , veAttendee = def
    , veCategories = def
    , veComment = def
    , veContact = def
    , veExDate = def
    , veRStatus = def
    , veRelated = def
    , veResources = def
    , veRDate = def
    , veAlarms = def
    , veOther = def
    }

fetchAndStore
    :: Members '[IO, StringTrace, HttpClient, AcidAccess EventStore] r
    => String -> Eff r ()
fetchAndStore start = do
    doc <- getDoc start
    links <- send . runX $ doc >>> css ".event h2 a" ! "href"
    mapM_ addEvent links
    nextPageUrl <- fmap (lookup "další") . send . runX $ doc >>> pagerLinks
    mapM_ fetchAndStore nextPageUrl
  where
    pagerLinks = css ".pager a" >>> (deep getText &&& getAttrValue "href")

magic
    :: Members '[IO, StringTrace, HttpClient, AcidAccess EventStore] r
    => Eff r ()
magic = do
    fetchAndStore "http://www.mestohudby.cz/calendar/all/list"
    (from, to) <- (mkFrom &&& mkTo) . utctDay <$> send getCurrentTime
    log $ "Rendering events from " <> show from <> " to " <> show to
    es <- query $ GetEvents
        (Just from)
        (Just to)
        ["klasicka-hudba", "jazz", "clubbing", "ostatni"]
        []
    log $ "Number of events to be rendered: " <> show (length es)
    send . BSL8.writeFile "bmh.ical" . printICalendar def $ def
        { vcEvents = Map.mapKeysMonotonic (flip (,) Nothing . fromString)
            $ Map.map musicEvent2VEvent es
        , vcOther = Set.singleton OtherProperty
            { otherName = "X-WR-CALNAME"
            , otherValue = Text.Lazy.encodeUtf8 "Brno – město hudby"
            , otherParams = def
            }
        }
  where
    setGregorianDay d = (\ (y, m, _) -> fromGregorian y m d) . toGregorian

    mkFrom = setGregorianDay 1 . addDays (-7)

    mkTo = setGregorianDay 31 . addDays 14

main :: IO ()
main = withEventStore $ \ st -> runM
    . runTraceIO
    . runHttpClient myGet
    . flip runReader st
    $ magic
  where
    myGet :: Members '[StringTrace, IO] r => String -> Eff r BSL.ByteString
    -- myGet = cached "web_cache" $ delayed 5 . get
    myGet = delayed 5 . get
