{-# LANGUAGE ConstraintKinds #-}
module Ohua.Types.Computation where

import Universum

import Control.Monad.Logger
import Control.Monad.Error.Class hiding (Error)

import Control.Monad.Trans.Control (MonadBaseControl) -- TODO find out if this is really needed

type Error = Text

type CompM m = 
     ( MonadIO m
     , MonadBaseControl IO m
     , MonadError Error m
     , MonadLogger m
     , MonadLoggerIO m
     )
