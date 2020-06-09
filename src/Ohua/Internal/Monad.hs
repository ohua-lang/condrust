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
{-# LANGUAGE TemplateHaskell #-}

module Ohua.Internal.Monad where

import Universum

import qualified Control.Monad.RWS.Lazy
import qualified Control.Monad.RWS.Strict
import qualified Control.Monad.State.Lazy
import qualified Control.Monad.State.Strict
import Control.Monad.Writer (WriterT)
import Control.Monad.Error.Class hiding (Error)
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Vector as V
import Control.Lens.Operators ((%=), (.=))
import Control.Lens.TH

import Ohua.Types.Error
import Ohua.Types.Reference
import Ohua.Types.Make


-- | Stateful name generator
declareLenses [d|
    data NameGenerator = NameGenerator
        { takenNames :: !(HS.HashSet Binding)
        , simpleNameList :: [Binding]
        }
  |]

type instance SourceType NameGenerator = (HS.HashSet Binding, [Binding])

instance UnsafeMake NameGenerator where
    unsafeMake = uncurry NameGenerator

instance Make NameGenerator where
    make = pure . unsafeMake


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
