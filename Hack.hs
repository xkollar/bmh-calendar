{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hack where

import Network.URI (URI, URIAuth)
import Data.SafeCopy (deriveSafeCopy, base)

deriveSafeCopy 0 'base ''URI
deriveSafeCopy 0 'base ''URIAuth

