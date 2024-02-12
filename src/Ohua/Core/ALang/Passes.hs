-- |
-- Module      : $Header$
-- Description : Passes over algorithm language terms to ensure certain invariants
-- Copyright   : (c) Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable
--
-- This module implements a set of passes over ALang which perform
-- various tasks. The most important function is `normalize`, which
-- transforms an arbitrary ALang expression either into the normal
-- form of a sequence of let bindings which are invocations of
-- stateful functions on local or environment variables finalised by a
-- local binding as a return value.
-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Ohua.Core.ALang.Passes where

import Ohua.Core.Prelude

import Control.Monad.RWS.Lazy (evalRWST)
import Control.Monad.Writer (listen, runWriter, tell)
import Data.Functor.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import Ohua.Core.ALang.Lang
import Ohua.Core.ALang.PPrint
import Ohua.Core.ALang.Passes.Control (fusionPasses)
import Ohua.Core.ALang.Passes.If
import Ohua.Core.ALang.Passes.Smap
import Ohua.Core.ALang.Passes.Unit
import Ohua.Core.ALang.Passes.Literal
import Ohua.Core.ALang.Passes.State
import qualified Ohua.Core.InternalFunctions as IFuns
import Ohua.Core.Stage


runCorePasses :: (MonadOhua m, Show embExpr) => Expr embExpr ty -> m (Expr embExpr ty)
runCorePasses expr = do
    litE <- literalsToFunctions expr
    stage literalsALang expr

    let exprE = mkUnitFunctionsExplicit litE
    stage unitFunctionsALang exprE

    stateThreadsE <- preControlPasses exprE
    stage preControlSTCLangALang stateThreadsE

    smapE <- smapRewrite stateThreadsE
    stage smapTransformationALang smapE

    ifE <- ifRewrite smapE
    stage conditionalsTransformationALang ifE

    normalizedE <- normalize ifE
    stage normalizeAfterCorePasses normalizedE

    let stateThreadsE' = postControlPasses normalizedE
    stage postControlSTCLangALang stateThreadsE'

    uniqueCtrlsE <- fusionPasses stateThreadsE'
    stage uniqueCtrlsALang uniqueCtrlsE

    return uniqueCtrlsE

-- | Inline all references to lambdas.
-- Aka `let f = (\a -> E) in f N` -> `(\a -> E) N`
inlineLambdaRefs :: MonadOhua m => Expr embExpr ty -> m (Expr embExpr ty)
inlineLambdaRefs = flip runReaderT mempty . para go
  where
    go (LetF b (Lambda _ _, l) (_, body)) =
        l >>= \l' -> local (HM.insert b l') body
    go (VarF tbnd) = asks (fromMaybe (Var tbnd) . HM.lookup tbnd)
    go e = embed <$> traverse snd e

-- | Reduce lambdas by simulating application
-- Aka `(\a -> E) N` -> `let a = N in E`
-- Assumes lambda refs have been inlined
inlineLambda :: Expr embExpr ty -> Expr embExpr ty
inlineLambda =
    cata $ \case
        e@(ApplyF func argument) ->
            case func of
                Lambda assignment body -> Let assignment argument body
                Apply _ _ -> reduceLetCWith f func
                    where f (Lambda assignment body) =
                              Let assignment argument body
                          f v0 = Apply v0 argument
                _ -> embed e
        e -> embed e

-- recursively performs the substitution
--
-- let x = (let y = M in A) in E[x] -> let y = M in let x = A in E[x]
reduceLetA :: Expr embExpr ty -> Expr embExpr ty
reduceLetA =
    \case
        Let assign (Let assign2 val expr3) expr ->
            Let assign2 val $ reduceLetA $ Let assign expr3 expr
        e -> e

reduceLetCWith :: (Expr embExpr ty -> Expr embExpr ty) -> Expr embExpr ty -> Expr embExpr ty
reduceLetCWith f =
    \case
        Apply (Let assign val expr) argument ->
            Let assign val $ reduceLetCWith f $ Apply expr argument
        e -> f e

reduceLetC :: Expr embExpr ty -> Expr embExpr ty
reduceLetC = reduceLetCWith id

reduceAppArgument :: Expr embExpr ty -> Expr embExpr ty
reduceAppArgument =
    \case
        Apply function (Let assign val expr) ->
            Let assign val $ reduceApplication $ Apply function expr
        e -> e

-- recursively performs the substitution
--
-- (let x = M in A) N -> let x = M in A N
--
-- and then
--
-- A (let x = M in N) -> let x = M in A N
reduceApplication :: Expr embExpr ty -> Expr embExpr ty
reduceApplication = reduceLetCWith reduceAppArgument

-- | Lift all nested lets to the top level
-- Aka `let x = let y = E in N in M` -> `let y = E in let x = N in M`
-- and `(let x = E in F) a` -> `let x = E in F a`
letLift :: Expr embExpr ty -> Expr embExpr ty
letLift =
    cata $ \e ->
        let f =
                case e of
                    LetF{} -> reduceLetA
                    ApplyF _ _ -> reduceApplication
                    _ -> id
         in f $ embed e

-- -- | Inline all direct reassignments.
-- -- Aka `let x = E in let y = x in y` -> `let x = E in x`
inlineReassignments :: Expr embExpr ty -> Expr embExpr ty
inlineReassignments = flip runReader HM.empty . cata go
  where
    go (LetF bnd val body) =
        val >>= \v ->
            let requestReplace = local (HM.insert bnd v) body
             in case v of
                    Var {} -> requestReplace
                    Lit {} -> requestReplace
                    _ -> Let bnd v <$> body
    go (VarF val) = asks (fromMaybe (Var val) . HM.lookup val)
    go e = embed <$> sequence e

-- | Transforms the final expression into a let expression with the result variable as body.
-- Aka `let x = E in some/sf a` -> `let x = E in let y = some/sf a in y`
--
-- EDIT: Now also does the same for any residual lambdas
ensureFinalLet :: MonadOhua m => Expr embExpr ty -> m (Expr embExpr ty)
ensureFinalLet = ensureFinalLetInLambdas >=> ensureFinalLet'

-- | Transforms the final expression into a let expression with the result variable as body.
ensureFinalLet' :: MonadOhua m => Expr embExpr ty -> m (Expr embExpr ty)
ensureFinalLet' =
    para $ \case
        LetF b (oldV, _) (_, recB) -> Let b oldV <$> recB -- Recurse only into let body, not the bound value
        any0
            | isVarOrLambdaF any0 -> embed <$> traverse snd any0 -- Don't rebind a lambda or var. Continue or terminate
            | otherwise -> do -- Rebind anything else
                newBnd <- generateBinding
                let expr' = embed $ fmap fst any0
                let newTy = exprType expr'
                pure $ Let (TBind newBnd newTy) (embed $ fmap fst any0) (Var (TBind newBnd newTy))
  where
    isVarOrLambdaF =
        \case
            VarF _ -> True
            LambdaF {} -> True
            _ -> False

ensureFinalLetInLambdas :: MonadOhua m => Expr embExpr ty -> m (Expr embExpr ty)
ensureFinalLetInLambdas =
    cata $ \case
        LambdaF bnd body -> Lambda bnd <$> (ensureFinalLet' =<< body)
        a -> embed <$> sequence a

ensureAtLeastOneCall :: MonadGenBnd m => Expr embExpr ty -> m (Expr embExpr ty)
ensureAtLeastOneCall e@(Var (TBind _bnd ety)) = do
    newBnd <- generateBinding
    pure $ Let (TBind newBnd ety) (pureFunction IFuns.id (FunType (Right $ ety :| []) ety )`Apply` e) $ Var (TBind newBnd ety)
ensureAtLeastOneCall e = cata f e
  where
    f (LambdaF tbnd body) =
        body >>= \case
            v@(Var (TBind _bnd vty)) -> do
                newBnd <- generateBinding
                pure $
                    Lambda tbnd $
                    Let (TBind newBnd vty) (pureFunction IFuns.id (FunType (Right $ vty :| []) vty ) `Apply` v) $
                    Var (TBind newBnd vty)
            eInner -> pure $ Lambda tbnd eInner
    f eInner = embed <$> sequence eInner

-- | Removes bindings that are never used.
-- This is actually not safe becuase sfn invocations may have side effects
-- and therefore cannot be removed.
-- Assumes ssa for simplicity
removeUnusedBindings :: Expr embExpr ty -> Expr embExpr ty
removeUnusedBindings = fst . runWriter . cata go
  where
    go (VarF (TBind val ty)) = tell (HS.singleton val) >> return (Var (TBind val ty))
    go (LetF (TBind b tb) val body) = do
        (inner, used) <- listen body
        if not $ b `HS.member` used
            then return inner
            else do
                val' <- val
                pure $ Let (TBind b tb) val' inner
    go e = embed <$> sequence e

newtype MonoidCombineHashMap k v =
    MonoidCombineHashMap (HashMap k v)
    deriving (Show, Eq, Ord)

instance (Semigroup v, Eq k, Hashable k) =>
         Semigroup (MonoidCombineHashMap k v) where
    MonoidCombineHashMap m1 <> MonoidCombineHashMap m2 =
        MonoidCombineHashMap $ HM.unionWith (<>) m1 m2

instance (Semigroup v, Eq k, Hashable k) =>
         Monoid (MonoidCombineHashMap k v) where
    mempty = MonoidCombineHashMap mempty

data WasTouched
    = No
    | Yes
    deriving (Show, Eq, Ord)

instance Semigroup WasTouched where
    (<>) = max

instance Monoid WasTouched where
    mempty = No

type TouchMap = MonoidCombineHashMap Binding (WasTouched, WasTouched)


wasTouchedAsFunction :: TypedBinding ty  -> TouchMap
wasTouchedAsFunction (TBind bnd _ty) = MonoidCombineHashMap $ HM.singleton bnd (Yes, No)

wasTouchedAsValue :: TypedBinding ty -> TouchMap
wasTouchedAsValue (TBind bnd _ty) = MonoidCombineHashMap $ HM.singleton bnd (No, Yes)

lookupTouchState :: TypedBinding ty -> TouchMap -> (WasTouched, WasTouched)
lookupTouchState (TBind bnd _ty) (MonoidCombineHashMap m) =
    fromMaybe mempty $ HM.lookup bnd m

-- | Reduce curried expressions.  aka `let f = some/sf a in f b`
-- becomes `some/sf a b`.  It both inlines the curried function and
-- removes the binding site.  Recursively calls it self and therefore
-- handles redefinitions as well.  It only substitutes vars in the
-- function positions of apply's hence it may produce an expression
-- with undefined local bindings.  It is recommended therefore to
-- check this with 'noUndefinedBindings'.  If an undefined binding is
-- left behind which indicates the source expression was not
-- fulfilling all its invariants.
removeCurrying ::
       forall m embExpr ty. MonadError Error m
    => Expr embExpr ty
    -> m (Expr embExpr ty)
removeCurrying e = fst <$> evalRWST (para inlinePartials e) mempty ()
  where
    inlinePartials (LetF tBnd (_, val) (_, body)) = do
        val' <- val
        (body', touched) <- listen $ local (HM.insert (asBnd tBnd) val') body
        case lookupTouchState tBnd touched of
            (Yes, Yes) ->
                throwErrorDebugS $
                "Binding was used as function and value " <> show tBnd
            (Yes, _) -> pure body'
            _ -> pure $ Let tBnd val' body'
    inlinePartials (ApplyF (Var tbnd, _) (_, arg)) = do
        tell $ wasTouchedAsFunction tbnd
        val <- asks (HM.lookup $ asBnd tbnd)
        Apply <$>
            maybe
                (failWith $ "No suitable value found for binding " <> show tbnd <> 
                    "\n   in expression:\n" <> show  e)
                pure
                val <*>
            arg
    inlinePartials (VarF tbnd) = tell (wasTouchedAsValue tbnd) >> pure (Var tbnd)
    inlinePartials innerExpr = embed <$> traverse snd innerExpr

-- | Ensures the expression is a sequence of let statements terminated
-- with a local variable.
hasFinalLet :: MonadOhua m => Expr embExpr ty -> m ()
hasFinalLet =
    cata $ \case
        LetF _ _ body -> body
        VarF {} -> return ()
        _ -> failWith "Final value is not a var"


{-
-- | Ensures all of the optionally provided stateful function ids are unique.
noDuplicateIds :: MonadError Error m => Expr embExpr ty -> m ()
noDuplicateIds = flip evalStateT mempty . cata go
  where
    go (PureFunctionF _ (Just funid)) = do
        isMember <- gets (HS.member funid)
        when isMember $ failWith $ "Duplicate id " <> show funid
        modify (HS.insert funid)
    go e = sequence_ e
-}

-- | Checks that no apply to a local variable is performed.  This is a
-- simple check and it will pass on complex expressions even if they
-- would reduce to an apply to a local variable.
applyToPureFunction :: MonadOhua m => Expr embExpr ty -> m ()
applyToPureFunction =
    para $ \case
        ApplyF (Var bnd, _) _ ->
            failWith $ "Illegal Apply to local var " <> show bnd
        e -> mapM_ snd e

-- | Checks that all local bindings are defined before use.
-- Scoped. Aka bindings are only visible in their respective scopes.
-- Hence the expression does not need to be in SSA form.
noUndefinedBindings :: MonadOhua m => Expr embExpr ty -> m ()
noUndefinedBindings = flip runReaderT mempty . cata go
  where
    go (LetF tb val body) = val >> registerBinding (asBnd tb) body
    go (VarF tbnd) = do
        isDefined <-  asks (HS.member $ asBnd tbnd)
        unless isDefined $ failWith $ "Not in scope " <> show tbnd
        
    go (LambdaF tb body) = registerBinding (asBnd tb) body
    go e@(LitF _a ) = sequence_ e
    go e@(ApplyF _a _b ) = sequence_ e
    go e@(BindStateF _a _b ) = sequence_ e 
    -- go (Li)
    --go e = sequence_ e
    registerBinding b = (local . HS.insert) b

checkProgramValidity :: MonadOhua m => Expr embExpr ty -> m ()
checkProgramValidity e = do
    hasFinalLet e
    -- noDuplicateIds e
    applyToPureFunction e
    noUndefinedBindings e

-- | Lifts something like @if (f x) a b@ to @let x0 = f x in if x0 a b@
liftApplyToLetArgsIn :: MonadOhua m => Expr embExpr ty -> m (Expr embExpr ty)
liftApplyToLetArgsIn =
    lrPrewalkExprM $ \case
        Apply fn arg@(Apply _ _) -> do
            bnd <- generateBinding
            let argTy = exprType arg    
            return $ Let (TBind bnd argTy) arg $ Apply fn (Var (TBind bnd argTy))
        a -> return a

-- normalizeBind :: (MonadError Error m, MonadGenBnd m) => Expr embExpr ty -> m (Expr embExpr ty)
-- normalizeBind =
--     rewriteM $ \case
--         BindState e2 e1@(PureFunction _ _) ->
--             case e2 of
--                 Var _ -> pure Nothing
--                 Lit _ -> pure Nothing
--                 _ ->
--                     generateBinding >>= \b ->
--                         pure $ Just $ Let b e2 (BindState (Var b) e1)
--         BindState _ _ -> throwError "State bind target must be a pure function reference"
--         _ -> pure Nothing
dumpNormalizeDebug :: Bool
dumpNormalizeDebug = False

putStrLnND :: (Print str, MonadIO m) => str -> m ()
putStrLnND =
    if dumpNormalizeDebug
        then putStrLn
        else const $ return ()

printND :: (Show a, MonadIO m) => a -> m ()
printND =
    if dumpNormalizeDebug
        then print
        else const $ return ()

-- The canonical composition of the above transformations to create a
-- program with the invariants we expect.
normalize :: MonadOhua m => Expr embExpr ty -> m (Expr embExpr ty)
normalize e =
    (inlineReassignments <$>
    (reduceLambdas (letLift e) >>=
      (\ a ->
         putStrLnND ("Reduced lamdas" :: Text) >> printND (pretty a) >>
           return a))) >>=
    removeCurrying >>=
    (\a ->
         putStrLnND ("Removed Currying" :: Text) >> printND (pretty a) >>
         return a) >>=
    liftApplyToLetArgsIn >>=
    (\a -> putStrLnND ("App to App" :: Text) >> printND (pretty a) >> return a) .
    letLift >>=
    ensureFinalLet . inlineReassignments >>=
    ensureAtLeastOneCall
    -- we repeat this step until a fix point is reached.
    -- this is necessary as lambdas may be input to lambdas,
    -- which means after inlining them we may be able again to
    -- inline a ref and then inline the lambda.
    -- I doubt this will ever do more than two or three iterations,
    -- but to make sure it accepts every valid program this is necessary.
  where
    reduceLambdas expr = do
        res <- letLift . inlineLambda <$> inlineLambdaRefs expr
        if res == expr
            then return res
            else reduceLambdas res
            
