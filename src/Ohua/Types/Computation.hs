{-# LANGUAGE ConstraintKinds #-}
module Ohua.Types.Computation
    ( MonadGenBnd(generateBinding, generateBindingWith)
    , MonadIO(liftIO)
    , MonadError(throwError, catchError)
    , MonadLogger, LoggingT, runStderrLoggingT, runLoggingT, filterLogger
    , MonadLoggerIO(askLoggerIO)
    , LogLevel(..), LogSource, logDebugN, logInfoN
    , logWarnN, logErrorN, logOtherN
    , CompM
    -- ** Helper functions for building instances of 'MonadGenBnd'
    , GenBndT, runGenBndT
    , generateBindingIn, generateBindingWithIn
    , generateBindingFromGenerator, generateBindingFromGeneratorWith
    , initNameGen
    ) where

import Universum

import Control.Monad.Logger
import Control.Monad.Error.Class hiding (Error)

import Ohua.Internal.Monad
import Ohua.Types.Error


type CompM m = 
     ( MonadIO m
     , MonadGenBnd m
     , MonadError Error m
     , MonadLogger m
     )

runCompM :: LogLevel -> ExceptT Error (LoggingT IO) a -> IO a
runCompM targetLevel c =
    runStderrLoggingT $
    filterLogger (\_ level -> level >= targetLevel) $
    runExceptT c >>= either exitError pure
  where
    exitError message = do
        logErrorN message
        exitFailure
