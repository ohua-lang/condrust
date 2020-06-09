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

module Ohua.Internal.Monad where

import Ohua.Prelude

import Ohua.Environment

import qualified Control.Monad.RWS.Lazy
import Control.Monad.RWS.Strict (RWST, evalRWST)
import qualified Control.Monad.State.Lazy
import qualified Control.Monad.State.Strict
import Control.Monad.Writer (WriterT)
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Vector as V
import Control.Lens
import Control.Lens.Operators ((%=), (.=))


-- The compiler monad.
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

class Monad m => MonadGenBnd m where
    generateBinding :: m Binding
    default generateBinding :: (MonadGenBnd n, MonadTrans t, t n ~ m) =>
        m Binding
    generateBinding = lift generateBinding
    generateBindingWith :: Binding -> m Binding
    default generateBindingWith :: ( MonadGenBnd n
                                   , MonadTrans t
                                   , t n ~ m
                                   ) =>
        Binding -> m Binding
    generateBindingWith = lift . generateBindingWith

deepseqM :: (Monad m, NFData a) => a -> m ()
deepseqM a = a `deepseq` pure ()

instance MonadGenBnd OhuaM where
    generateBinding = OhuaM $ generateBindingIn nameGenerator
    generateBindingWith = OhuaM . generateBindingWithIn nameGenerator

generateBindingFromGenerator :: NameGenerator -> (Binding, NameGenerator)
generateBindingFromGenerator g = (h, g')
  where
    taken = g ^. takenNames
    (h, t) =
        case dropWhile (`HS.member` taken) (g ^. simpleNameList) of
            (x:xs) -> (x, xs)
            [] -> error "Simple names is empty, this should be impossible"
    g' = g & simpleNameList .~ t & takenNames %~ HS.insert h

generateBindingFromGeneratorWith ::
       Binding -> NameGenerator -> (Binding, NameGenerator)
generateBindingFromGeneratorWith prefixBnd g = (h, g')
  where
    prefix = unwrap prefixBnd
    taken = g ^. takenNames
    prefix' = prefix <> "_"
    h = fromMaybe (error "IMPOSSIBLE") $
        safeHead $
        dropWhile (`HS.member` taken) $
        map (makeThrow . (prefix' <>) . show) ([0 ..] :: [Int])
    g' = g & takenNames %~ HS.insert h

generateBindingIn :: MonadState s m => Lens' s NameGenerator -> m Binding
generateBindingIn accessor = do
    (bnd, gen') <- generateBindingFromGenerator <$> use accessor
    accessor .= gen'
    pure bnd

generateBindingWithIn ::
       MonadState s m => Lens' s NameGenerator -> Binding -> m Binding
generateBindingWithIn accessor prefix = do
    (bnd, gen') <- generateBindingFromGeneratorWith prefix <$> use accessor
    accessor .= gen'
    pure bnd

instance (MonadGenBnd m, Monad m) => MonadGenBnd (ReaderT e m)

instance (MonadGenBnd m, Monad m, Monoid w) => MonadGenBnd (WriterT w m)

instance (MonadGenBnd m, Monad m) =>
         MonadGenBnd (Control.Monad.State.Strict.StateT s m)

instance (MonadGenBnd m, Monad m) =>
         MonadGenBnd (Control.Monad.State.Lazy.StateT s m)

instance (MonadGenBnd m, Monad m, Monoid w) =>
         MonadGenBnd (Control.Monad.RWS.Strict.RWST e w s m)

instance (MonadGenBnd m, Monad m, Monoid w) =>
         MonadGenBnd (Control.Monad.RWS.Lazy.RWST e w s m)

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
    -> (expr -> OhuaM result)
    -> expr
    -> (expr -> HS.HashSet Binding)
    -> LoggingT IO (Either Error result)
runFromExpr opts f tree getBnds =
    runFromBindings opts (f tree) (getBnds tree)

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

initNameGen :: MonadError Error m => HS.HashSet Binding -> m NameGenerator
initNameGen taken =
    make
        ( taken
        , [ makeThrow $ char `T.cons` maybe [] show num
          | num <- Nothing : map Just [(0 :: Integer) ..]
          , char <- ['a' .. 'z']
          ])

newtype GenBndT m a = GenBndT (StateT NameGenerator m a)
    deriving (Monad, Functor, Applicative, MonadTrans)

instance Monad m => MonadGenBnd (GenBndT m) where
    generateBinding = GenBndT $ generateBindingIn id
    generateBindingWith = GenBndT . generateBindingWithIn id

runGenBndT :: MonadError Error m => HS.HashSet Binding -> GenBndT m a -> m a
runGenBndT taken (GenBndT comp) = do
    ng <- initNameGen taken
    evaluatingStateT ng comp
