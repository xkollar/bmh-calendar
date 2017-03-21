{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
module FstOrd where

import Data.Eq (Eq, (==))
import Data.Function (on)
import Data.Ord (Ord, compare)
import Text.Show (Show)

import Data.SafeCopy (base, deriveSafeCopy)
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
