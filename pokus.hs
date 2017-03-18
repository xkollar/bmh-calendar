{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Arrow
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

import CachingGet (getCached)
import TimeHelper (readDate)


getDoc url = parseHtml' <$> getCached url
  where
    parseHtml' = parseHtml . Text.Lazy.unpack . Text.Lazy.decodeUtf8

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
        extract' = extractWith head
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

    uid = reverse . takeWhile isDigit $ reverse url

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

data MusicEvent = MusicEvent
    { meCreated :: UTCTime
    , meSummary :: String
    , meStart :: LocalTime
    , meUid :: String -- Most likely just number, or place"/"number
    , meUrl :: URI
    , meGenres :: [String] -- Like ["klasicka-hudba", "pop"]
    , mePlace :: String -- Like "Sono centrum"
    , meAddress :: String -- Like "Veveri 123, Brno"
    , meDescription :: String
    }
  deriving Show

exampleMusicEvent :: MusicEvent
exampleMusicEvent = MusicEvent
    { meCreated = read "1970-01-01 00:00:00"
    , meSummary = "Těžkej Pokondr"
    , meStart = read "2017-11-23 19:00:00"
    , meUid = "sono-centrum/event-9874"
    , meUrl = fromJust $ parseURI "http://www.mestohudby.cz/calendar/sono-centrum/event-9874"
    , meGenres = ["rock", "pop", "ostatni"]
    , mePlace = "Sono Centrum"
    , meAddress = "Veveří 113, Brno"
    , meDescription =
        "Miloš Pokorný(1964) a Roman Ondráček(1966) odstartovali svou kariéru\
        \ v roce 1991 na Bonton rádiu…"
    }

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
