{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Effects.Acid where

import Control.Monad ((>>=))
import System.IO (IO)

import Control.Monad.Freer (Eff, Members, send)
import Control.Monad.Freer.Reader (Reader, ask)
import qualified Data.Acid as Acid
import Data.Acid.Core (MethodState)


type AcidAccess a = Reader (Acid.AcidState a)

query
    :: (Members '[IO, AcidAccess (MethodState event)] effs
        , Acid.QueryEvent event)
    => event -> Eff effs (Acid.EventResult event)
query q = ask >>= \ st -> send (Acid.query st q)

update
    :: (Members '[IO, AcidAccess (MethodState event)] effs
        , Acid.UpdateEvent event)
    => event -> Eff effs (Acid.EventResult event)
update q = ask >>= \ st -> send (Acid.update st q)
