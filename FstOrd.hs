{-# LANGUAGE TemplateHaskell #-}
module FstOrd where

import Data.Function (on)

import Data.SafeCopy
import Data.Typeable (Typeable)

data FstOrd a b = FstOrd
    { _fst :: !a
    , _snd :: !b
    }
  deriving (Show, Typeable)

instance Eq a => Eq (FstOrd a b) where
    (==) = (==) `on` _fst

instance Ord a => Ord (FstOrd a b) where
    compare = compare `on` _fst

deriveSafeCopy 0 'base ''FstOrd
