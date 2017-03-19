{-# LANGUAGE OverloadedStrings #-}
module CachingGet (getCached) where

import Data.Monoid ((<>))

import Control.Lens ((&), (.~), (^.))
import Crypto.Hash.SHA1 (hash)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Network.Wreq
    (Options, checkStatus, cookies, defaults, getWith, headers, responseBody)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Text.Printf (printf)

import Delayed (delayed)

myOpts :: Options
myOpts = defaults
    & cookies .~ Nothing
    & checkStatus .~ noCheck
    & headers .~ [("User-Agent", "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:40.0) Gecko/20100101 Firefox/40.1")]
  where
    noCheck = Just $ \ _ _ _ -> Nothing

myGet :: String -> IO BSL.ByteString
myGet url = do
    res <- getWith myOpts url
    pure $ res ^. responseBody

cacheDir :: FilePath
cacheDir = "web_cache"

getCached :: String -> IO BSL.ByteString
getCached url = do
    let cacheFile = cacheDir </> (toHex . hash $ BS8.pack url)
    hit <- doesFileExist cacheFile
    if hit
        then do
            putStrLn $ "Cached hit: " <> url
            BSL.readFile cacheFile
        else do
            putStrLn $ "Cached miss: " <> url
            createDirectoryIfMissing True cacheDir
            writeFile (cacheFile <> ".url") url
            c <- delayed 2 (myGet url)
            BSL.writeFile cacheFile c
            pure c

toHex :: BS.ByteString -> String
toHex bytes = BS.unpack bytes >>= printf "%02x"
