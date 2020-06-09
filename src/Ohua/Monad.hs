-- |
-- Module      : $Header$
-- Description : The base Ohua compiler monad
-- Copyright   : (c) Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : POSIX
-- This source code is licensed under the terms described in the associated LICENSE.TXT file

module Ohua.Monad
    ( OhuaM, runFromExpr, runFromBindings
    , MonadGenId(generateId, resetIdCounter)
    , MonadGenBnd(generateBinding, generateBindingWith)
    , MonadReadEnvironment(getEnvironment), fromEnv
    , MonadIO(liftIO)
    , MonadError(throwError, catchError)
    , MonadLogger, LoggingT, runStderrLoggingT, runHandleLoggingT, runSilentLoggingT, runLoggingT, filterLogger
    , MonadLoggerIO(askLoggerIO)
    , LogLevel(..), LogSource, logDebugN, logInfoN
    , logWarnN, logErrorN, logOtherN
    , MonadOhua
    -- ** Helper functions for building instances of 'MonadGenBnd'
    , GenBndT, runGenBndT
    , generateBindingIn, generateBindingWithIn
    , generateBindingFromGenerator, generateBindingFromGeneratorWith
    , initNameGen
    ) where

import Ohua.Prelude hiding (hPutStr)

import System.Log.FastLogger (fromLogStr)
import Control.Monad.Error.Class (MonadError, throwError, catchError)
import Data.ByteString (hPutStr)

import Ohua.Internal.Monad
import Ohua.Environment
import Ohua.Util


runSilentLoggingT :: LoggingT m a -> m a
runSilentLoggingT = flip runLoggingT $ \_ _ _ _ -> pure ()

fromEnv :: (MonadReadEnvironment m, Functor m) => Lens' Environment a -> m a
fromEnv l = view l <$> getEnvironment

runHandleLoggingT :: Handle -> LoggingT m a -> m a
runHandleLoggingT h = (`runLoggingT` output)
  where
    output loc src level msg = hPutStr h $ fromLogStr $ defaultLogStr loc src level msg
