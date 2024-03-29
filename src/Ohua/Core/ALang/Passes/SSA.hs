-- |
-- Module      : $Header$
-- Description : Transform an algorithm language term into single static assignment form
-- Copyright   : (c) Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable
-- This source code is licensed under the terms described in the associated LICENSE.TXT file

module Ohua.Core.ALang.Passes.SSA where

import Ohua.Core.Prelude

import Data.Functor.Foldable
import qualified Data.HashMap.Strict as HM
import Control.Category ((>>>))
import Control.Lens (non, at)

import Ohua.Core.ALang.Lang
import Ohua.Core.ALang.Util

type LocalScope ty = HM.HashMap (TypedBinding ty) (TypedBinding ty)

ssaResolve :: MonadReader (LocalScope ty) m => (TypedBinding ty) -> m (TypedBinding ty)
ssaResolve tbnd = view $ at tbnd . non tbnd

-- | Generate a new name for the provided binding and run the inner
-- computation with that name in scope to replace the provided binding
-- Returns both the generated binding and the result of the inner
-- computation
--
-- Passing in the computation which is to be executed in the modified
-- environment makes this function a bit harder to use (or rather the
-- code using it less readable) because it does a lot of passing
-- functions as arguments, however it very nicely encapsulates the
-- scope changes which means they will never leak from where they are
-- supposed to be applied
ssaRename ::
       (MonadGenBnd m, MonadReader (LocalScope ty) m)
    => (TypedBinding ty)
    -> ((TypedBinding ty) -> m a)
    -> m a
ssaRename old@(TBind oldBnd ty) cont = do
    newBnd <- generateBindingWith oldBnd
    let new = TBind newBnd ty
    local (HM.insert old new) $ cont new

performSSA :: MonadOhua m => Expr embExpr ty -> m (Expr embExpr ty)
performSSA = flip runReaderT mempty . ssa

ssa :: (MonadOhua m, MonadReader (LocalScope ty) m)
    => Expr embExpr ty
    -> m (Expr embExpr ty)
ssa =
    cata $ \case
        VarF tbnd -> Var <$> ssaResolve tbnd
        LambdaF v body -> ssaRename v $ \v' -> Lambda v' <$> body
        LetF v val body -> do
        -- REMINDER: As long as we can not distuish if `v` here is a normal vaiable
        --           or a recursive function we can only support either recursive function
        --           where `v`'s name in the RHS `val` has to be the same as on the left 
        --           or we can 'support' name shadowing in the code, where we needed to rename
        --           the RHS first i.e. `let v_new  = ... v_old ... in ...`
        -- For this distinction it might suffice to know, if `v` was an algorithm in the frontent. We shoulb be $
        -- able to learn this from the function types we gathered in the frontend
            --  val' <- val
            --  ssaRename v $ \v' -> Let v' val' <$> body
            ssaRename v $ \v' -> Let v' <$> val <*> body
        e -> embed <$> sequence e

            

-- Check if an expression is in ssa form. Returns @Nothing@ if it is
-- SSA Returns @Just aBinding@ where @aBinding@ is a binding which was
-- defined (at least) twice
isSSA :: Expr embExpr ty -> [(TypedBinding ty)]
isSSA e = [b | (b, count) <- HM.toList counts, count > 1]
  where
    counts = HM.fromListWith (+) [(b, 1 :: Word) | b <- definedBindings e]



checkSSA :: MonadOhua m => Expr embExpr ty -> m ()
checkSSA = isSSA >>> \case
    [] -> return ()
    other -> throwErrorDebugS $ mkMsg other
  where
    mkMsg bnd = "Redefinition of bindings " <> show bnd
