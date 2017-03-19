{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Arrow
import Control.Monad
import Data.Char (isDigit)
import Data.List
import Data.Maybe
import Data.Monoid ((<>))
import Data.String (fromString)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Default (def)
import Data.Text (Text)
import qualified Data.Text.Lazy as Text.Lazy (unpack)
import qualified Data.Text.Lazy.Encoding as Text.Lazy (decodeUtf8)
import Data.Time (getCurrentTime, UTCTime, LocalTime)
import Network.URI (URI, parseURI)
import Text.HandsomeSoup
import Text.ICalendar
import Text.XML.HXT.Core

import Data.Acid
import Data.Typeable

import CachingGet (getCached)
import EventStore
import MusicEvent (MusicEvent(..), Uid)
import TimeHelper (readDate)


getDoc url = parseHtml' <$> getCached url
  where
    parseHtml' = parseHtml . Text.Lazy.unpack . Text.Lazy.decodeUtf8

url2uid :: String -> Uid
url2uid = reverse . takeWhile isDigit . reverse

addEvent :: AcidState EventStore -> String -> IO ()
addEvent st url = do
    b <- query st . IsEvent $ url2uid url
    unless b $ do
        putStrLn $ "Fetching event " <> url
        fetchEvent url >>= update st . InsertEvent

ex = withEventStore $ \ st -> do
    let f url = do
            doc <- getDoc url
            links <- runX $ doc >>> css ".event h2 a" ! "href"
            mapM_ (addEvent st) links
            nextPageUrl <- fmap (lookup "další") . runX
                $ doc >>> css ".pager a" >>> (deep getText &&& getAttrValue "href")
            mapM_ f nextPageUrl
    f "http://www.mestohudby.cz/calendar/all/list"

exampfel :: IO ()
exampfel = f "http://www.mestohudby.cz/calendar/all/list" where
    f url = do
        doc <- getDoc url
        links <- runX $ doc >>> css ".event h2 a" ! "href"
        mapM_ putStrLn $ sort links
        mapM_ fetchEvent links
        nextPageUrl <- fmap (lookup "další") . runX
            $ doc >>> css ".pager a" >>> (deep getText &&& getAttrValue "href")
        mapM_ f nextPageUrl

fetchEvent :: String -> IO MusicEvent
fetchEvent url = do
    doc <- getDoc url
    let extractWith e f = fmap e . runX $ doc >>> f
        extract' = extractWith (fromMaybe "unknown" . listToMaybe)
        extract = extractWith id
    mkMusicEvent
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

    mkPlace = maybe "unspecified" snd . lookup "Lokace"

    mkGenres = map (cutGenre . fst . snd) . filter (("Žánr"==) . fst)

    cutGenre = reverse . takeWhile ('/'/=) . reverse

musicEvent2VEvent :: MusicEvent -> VEvent
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
