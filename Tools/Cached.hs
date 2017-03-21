{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Tools.Cached (cached) where

import Data.Monoid ((<>))

import Control.Monad.Freer (Eff, Members, send)
import Crypto.Hash.SHA1 (hash)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Text.Printf (printf)

import Effects.Trace (StringTrace, trace)


cached
    :: Members '[StringTrace, IO] r
    => FilePath
    -> (String -> Eff r BSL.ByteString)
    -> String
    -> Eff r BSL.ByteString
cached cacheDir action arg = do
    let cacheFile = cacheDir </> (toHex . hash $ BS8.pack arg)
    hit <- send $ doesFileExist cacheFile
    if hit
        then do
            trace $ "Cached hit: " <> arg
            send $ BSL.readFile cacheFile
        else do
            trace $ "Cached miss: " <> arg
            send $ createDirectoryIfMissing True cacheDir
            send $ writeFile (cacheFile <> ".arg") arg
            c <- action arg
            send $ BSL.writeFile cacheFile c
            pure c

toHex :: BS.ByteString -> String
toHex bytes = BS.unpack bytes >>= printf "%02x"
