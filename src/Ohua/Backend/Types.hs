module Ohua.Backend.Types where

import Ohua.Prelude

import Ohua.Types (Error)
import Control.Monad.Trans.Control (MonadBaseControl) -- TODO find out if this is really needed

-- TODO Put into common place and reuse throughout frontend, middle-end, backend.
type CompM m = 
     ( MonadIO m
     , MonadBaseControl IO m
     , MonadError Error m
     , MonadLogger m
     , MonadLoggerIO m
     )
