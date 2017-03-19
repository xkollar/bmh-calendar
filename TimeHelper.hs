{-# LANGUAGE NoImplicitPrelude #-}
module TimeHelper
    ( readDate
    , readDate'
    , midnightOf
    )
  where

import Control.Applicative ((<*), (<*>), pure)
import Control.Monad (Monad, (>>=), fail, return)
import Data.Char (isDigit, isSpace)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.List (dropWhile, dropWhileEnd, filter, null)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String (String)
import Data.Tuple (snd)
import Text.ParserCombinators.ReadP
    (ReadP, char, eof, munch, readP_to_S, skipSpaces)
import Text.Read (Read, read, reads)
import Text.Show (show)

import Data.Time (Day, LocalTime(..), midnight)

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

-- example :: [String]
-- example = ["\n                  1.3.2017, 19:30        \n        ","\n  "]

readM :: (Monad m, Read a) => String -> String -> m a
readM label s = case filter (null . snd) $ reads s of
    (x, _):_ -> pure x
    _ -> fail $ "Expected " <> label <> " but got: " <> show s

digits :: ReadP String
digits = munch isDigit

dateString :: ReadP String
dateString = mkDate
    <$> digits <* dot
    <*> digits <* dot
    <*> digits <* char ',' <* skipSpaces
    <*> digits <* colon
    <*> digits
  where
    mkDate day month year hour minute =
        year <> "-" <> align month <> "-" <> align day
            <> " " <> align hour <> ":" <> align minute <> ":00"

    align [x] = ['0',x]
    align s = s

    dot = char '.'

    colon = char ':'

dateP :: ReadP LocalTime
dateP = dateString >>= readM "date"

readDate :: Monad m => String -> m LocalTime
readDate s = case readP_to_S (dateP <* eof) $ trim s of
    [(x,_)] -> return x
    [] -> fail $ "Error parsing date. No parse for " <> show s
    _ -> fail $ "Error parsing date. Multiple parses for " <> show s

readDate' :: String -> LocalTime
readDate' = fromMaybe (read "1970-01-01 00:00:00") . readDate

midnightOf :: Day -> LocalTime
midnightOf d = LocalTime
    { localDay = d
    , localTimeOfDay = midnight
    }

