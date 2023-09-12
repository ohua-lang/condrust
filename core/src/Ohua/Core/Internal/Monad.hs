-- |
-- Module      : $Header$
-- Description : The base Ohua compiler monad
-- Copyright   : (c) Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : POSIX
-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}

module Ohua.Core.Internal.Monad where

import Ohua.Prelude

import qualified Control.Monad.RWS.Lazy
import Control.Monad.RWS.Strict (RWST, evalRWST)
import qualified Control.Monad.State.Lazy
import qualified Control.Monad.State.Strict
import Control.Monad.Writer (WriterT)
import qualified Data.HashSet as HS
import Control.Lens


import Ohua.Core.Types as Ty
import Ohua.Core.ALang.Lang


-- The core compiler monad.
-- Encapsulates the state necessary to generate bindings
-- Allows IO actions.
-- In development this collects errors via a MonadWriter, in production this collection will
-- be turned off and be replaced by an exception, as such errors should technically not occur
-- there
newtype OhuaM a = OhuaM
    { runOhuaM :: RWST Environment () OhuaState (ExceptT Error (LoggingT IO)) a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadError Error
               , MonadLogger
               , MonadLoggerIO
               )

instance MonadGenBnd OhuaM where
    generateBinding = OhuaM $ generateBindingIn nameGenerator
    generateBindingWith = OhuaM . generateBindingWithIn nameGenerator

class MonadGenId m where
    generateId :: m FnId
    default generateId :: (MonadGenId n, Monad n, MonadTrans t, t n ~ m) =>
        m FnId
    generateId = lift generateId
    -- | Unsafe. Only use this if yoy know what you are doing!!
    resetIdCounter :: FnId -> m ()
    default resetIdCounter :: (MonadGenId n, Monad n, MonadTrans t, t n ~ m) =>
        FnId -> m ()
    resetIdCounter = lift . resetIdCounter

instance MonadGenId OhuaM where
    generateId =
        OhuaM $ do
            idCounter %= succ
            use idCounter
    resetIdCounter val = OhuaM $ idCounter .= val

instance (MonadGenId m, Monad m) => MonadGenId (ReaderT e m)

instance (MonadGenId m, Monad m, Monoid w) => MonadGenId (WriterT w m)

instance (MonadGenId m, Monad m) =>
         MonadGenId (Control.Monad.State.Strict.StateT s m)

instance (MonadGenId m, Monad m) =>
         MonadGenId (Control.Monad.State.Lazy.StateT s m)

instance (MonadGenId m, Monad m, Monoid w) =>
         MonadGenId (Control.Monad.RWS.Lazy.RWST e w s m)

instance (MonadGenId m, Monad m, Monoid w) =>
         MonadGenId (Control.Monad.RWS.Strict.RWST e w s m)

class MonadReadEnvironment m where
    getEnvironment :: m Environment
    default getEnvironment :: ( MonadTrans t
                              , Monad n
                              , MonadReadEnvironment n
                              , t n ~ m
                              ) =>
        m Environment
    getEnvironment = lift getEnvironment

instance MonadReadEnvironment OhuaM where
    getEnvironment = OhuaM ask

instance (MonadReadEnvironment m, Monad m) =>
         MonadReadEnvironment (ReaderT e m)

instance (MonadReadEnvironment m, Monad m) =>
         MonadReadEnvironment (Control.Monad.State.Lazy.StateT s m)

instance (MonadReadEnvironment m, Monad m) =>
         MonadReadEnvironment (Control.Monad.State.Strict.StateT s m)

instance (MonadReadEnvironment m, Monad m, Monoid w) =>
         MonadReadEnvironment (WriterT w m)

instance (MonadReadEnvironment m, Monad m, Monoid w) =>
         MonadReadEnvironment (Control.Monad.RWS.Lazy.RWST e w s m)

instance (MonadReadEnvironment m, Monad m, Monoid w) =>
         MonadReadEnvironment (Control.Monad.RWS.Strict.RWST e w s m)

type MonadOhua m
     = ( MonadGenId m
       , MonadGenBnd m
       , MonadError Error m
       , MonadIO m
       , MonadReadEnvironment m
       , MonadLogger m)

-- | Run a compiler
-- Creates the state from the tree being passed in
-- If there are any errors during the compilation they are reported together at the end
runFromExpr ::
       Options
    -> (Expr ty -> OhuaM result)
    -> Expr ty
    -> LoggingT IO (Either Error result)
runFromExpr opts f tree =
    runFromBindings opts (f tree) $ HS.fromList [b | Var (TBind b _t) <- universe tree]

-- | Run a compiler
-- Creates the state from the tree being passed in
-- If there are any errors during the compilation they are reported together at the end
runFromExprAndType ::
    Options -> 
    -- | a (compiler step) function on an algorithm and it's return type
    (HostType ty -> Expr ty -> OhuaM result) ->
    -- | an algorithm in its current compile step representation
    Expr ty -> 
    -- | the algorithms output type
    HostType ty ->
    LoggingT IO (Either Error result)
runFromExprAndType opts f tree ty =
    runFromBindings opts (f ty tree) $ HS.fromList [b | Var (TBind b _t) <- universe tree]

runFromBindings ::
       Options
    -> OhuaM result
    -> HS.HashSet Binding
    -> LoggingT IO (Either Error result)
runFromBindings opts f taken = runExceptT $ do
    s0 <- make =<< ((,) <$> initNameGen taken <*> make 0)
    fst <$> evalRWST (runOhuaM f) env s0
  where
    env = def & options .~ opts
