{-# LANGUAGE ConstraintKinds #-}
module Ohua.Types.Computation where

import Ohua.Prelude

import Control.Monad.Trans.Control (MonadBaseControl) -- TODO find out if this is really needed

type Error = Text

type CompM m = 
     ( MonadIO m
     , MonadBaseControl IO m
     , MonadError Error m
     , MonadLogger m
     , MonadLoggerIO m
     )
