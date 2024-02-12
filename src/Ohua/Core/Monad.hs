{-# LANGUAGE CPP #-}

#include "compat.h"

module Ohua.Core.Monad
    ( OhuaM, runFromExpr, runFromBindings, runFromExprAndType
    , MonadGenId(generateId, resetIdCounter)
    , MonadGenBnd(generateBinding, generateBindingWith)
    , MonadReadEnvironment(getEnvironment), fromEnv
    , MonadIO(liftIO)
    , MonadError(throwError, catchError), failWith
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

import Ohua.Commons.Prelude hiding (hPutStr)

-- import System.Log.FastLogger (fromLogStr)
-- import Control.Monad.Error.Class (MonadError, throwError, catchError)
import Data.ByteString (hPutStr)

import Ohua.Core.Internal.Monad
import Ohua.Core.Types
import Ohua.Core.Util

runSilentLoggingT :: LoggingT m a -> m a
runSilentLoggingT = flip runLoggingT $ \_ _ _ _ -> pure ()

-- | Alias for backwards compatibility with old `MonadOhua` interface
failWith :: (HasCallStack, MonadError Error m) => Error -> m a
failWith = throwErrorDebugS


fromEnv :: (MonadReadEnvironment m, Functor m) => Lens' Environment a -> m a
fromEnv l = view l <$> getEnvironment

runHandleLoggingT :: Handle -> LoggingT m a -> m a
runHandleLoggingT h = (`runLoggingT` output)
  where
    output loc src level msg = hPutStr h $ fromLogStr $ defaultLogStr loc src level msg
