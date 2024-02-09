{-# LANGUAGE ConstraintKinds #-}
module Ohua.Commons.Types.Computation
    ( NameGenerator
    , simpleNameList
    , takenNames
    , MonadGenBnd(generateBinding, generateBindingWith)
    , MonadIO(liftIO)
    , MonadError(throwError, catchError)
    , MonadLogger, LoggingT, runStderrLoggingT, runLoggingT, filterLogger
    , MonadLoggerIO(askLoggerIO)
    , LogLevel(..), LogSource, logDebugN, logInfoN
    , logWarnN, logErrorN, logOtherN
    , ErrAndLogM, runErrAndLogM
    -- ** Helper functions for building instances of 'MonadGenBnd'
    , GenBndT, runGenBndT
    , generateBindingIn, generateBindingWithIn
    , generateBindingFromGenerator, generateBindingFromGeneratorWith
    , initNameGen
    ) where

import Universum

import Control.Monad.Logger
import Control.Monad.Error.Class hiding (Error)

import Ohua.Commons.Internal.Monad
import Ohua.Commons.Types.Error


type ErrAndLogM m = ( MonadError Error m, MonadLoggerIO m )

runErrAndLogM :: LogLevel -> ExceptT Error (LoggingT IO) a -> IO a
runErrAndLogM targetLevel c =
    runStderrLoggingT $
    filterLogger (\_ level -> level >= targetLevel) $
    runExceptT c >>= either exitError pure
  where
    exitError message = do
        logErrorN message
        exitFailure
