-- |
-- Module      : $Header$
-- Description : Lowering from ALang to DFLang.
-- Copyright   : (c) Sebastian Ertel 2020. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com
-- Stability   : experimental
-- Portability : portable
-- This source code is licensed under the terms described in the associated LICENSE.TXT file
--
-- Passes required to transform an expression in ALang into an expression in DFLang.
--
module Ohua.Core.DFLang.Passes where

import Ohua.Core.Prelude

import Control.Lens (at, non)
import Control.Monad (msum)
import Control.Monad.Writer (MonadWriter, runWriterT, tell)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.IntMap.Strict as IM
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.List.NonEmpty as NE
import qualified Data.List.NonEmpty.Extra as NEE

import Control.Monad.Writer.Lazy (censor)

import Ohua.Core.ALang.Lang
import Ohua.Core.ALang.Util

import Ohua.Core.DFLang.Lang


runCorePasses :: (MonadOhua m) => NormalizedExpr -> m NormalizedDFExpr
runCorePasses = removeNth

-- I really should not have to do this in the first place.
-- All transformations that need an Nth node because they introduce functions whose output
-- needs destructuring, should be implemented on top of NormalizedDFExpr.

-- Currently, this code does not cover destructurings of destructurings but this is ok, because
-- we do not create those.
removeNth :: MonadOhua m => NormalizedExpr -> m NormalizedDFExpr
removeNth expr = do
    checkSSA expr
    let exp' = evalState (f expr) HM.empty
    return exp'
    where
        -- Assumption: Expression is in SSA form, so every site is uniquely identified by its output binding.     

        -- explicit traversal prevents non-exhaustive pattern warnings and allows to convert stuff to Coq later on.
        f = \case 
                (LetPureFun app cont) -> go app cont LetPureDFFun
                (LetStateFun app cont) -> go app cont LetStateDFFun
                (VarFun bnd) -> pure $ VarDFFun bnd
            where 
                go :: App a 
                    -> NormalizedExpr 
                    -> (DFApp a -> NormalizedDFExpr -> NormalizedDFExpr) 
                    -> State (HM.HashMap Binding (NonEmpty (Integer, Binding))) NormalizedDFExpr
                go app cont g = do 
                    cont' <- f cont
                    app' <- toDFAppFun app
                    pure $ maybe cont' (`g` cont') app'

        -- Note how this makes sure to preserve the semantics of the functions!
        -- TODO we would normally say that the binding does not change its type!
        toDFAppFun :: App a -> State (HM.HashMap Binding (NonEmpty (Integer, Binding))) (Maybe (DFApp a))
        toDFAppFun (PureFun tgt "ohua.lang/nth" [DFEnvVar (NumericLit i),_ , DFVar (DataBinding src)]) =
            modify (HM.insertWith (<>) src ((i,unwrapABnd tgt) :| [])) >> pure Nothing
        toDFAppFun (PureFun out fun ins) = do
            hm <- get
            let out' = toDFOuts (unwrapABnd out) DataBinding hm
            return $ Just $ PureDFFun out' fun ins
        toDFAppFun (StateFun (stateOut,out) fun stateIn ins) = do
            hm <- get
            let out' = toDFOuts (unwrapABnd out) DataBinding hm
            let stateOut' = (\s -> toDFOuts (unwrapABnd s) StateBinding hm) <$> stateOut
            return $ Just $ StateDFFun (stateOut',out') fun stateIn ins
        toDFOuts :: Binding -> (Binding -> ABinding a) -> HM.HashMap Binding (NonEmpty (Integer, Binding)) -> OutData a
        toDFOuts out bndFun = maybe
                -- TODO normally I would not have to unwrap the bindings here but they would preserve their
                --      annotations and therewith make sure that they do not lose their semantics!
                (Direct $ bndFun out)
                (Destruct . NE.map (Direct . bndFun . snd) . NEE.sortOn fst) . 
                HM.lookup out


-- | This pass makes sure no function application is using a binding that has not been defined.
--   TODO: This is once more something that we should enforce via the type system or Liquid Haskell!
checkDefinedUsage :: MonadOhua m => NormalizedDFExpr -> m ()
checkDefinedUsage expr = void $ evalStateT (descendM f expr) HS.empty
    where
        f e@(LetPureDFFun app cont) = checkAndDescend app cont >> return e
        f e@(LetStateDFFun app cont) = checkAndDescend app cont >> return e
        f e = return e
        checkAndDescend :: (MonadOhua m, MonadState (HS.HashSet Binding) m) => DFApp a -> NormalizedDFExpr -> m ()
        checkAndDescend app cont = do
            let ins = insDFApp app
            let outs = outsDFApp app
            defined <- get
            mapM_ (failWith . ("Undefined binding:" <> ) . show) $
                filter (not . (`HS.member` defined)) ins
            put $ HS.union defined $ HS.fromList $ NE.toList outs
            void $ descendM f cont 

-- FIXME This thing should be a type level annotation, so we do not have to verify 
--       this anymore at runtime! (Use a function to tag an expression with SSA. 
--       The tag is lost whenever a new binding is introduced.)
-- | Check that a sequence of let expressions does not redefine bindings.
checkSSA :: MonadOhua m => NormalizedExpr -> m ()
checkSSA e = 
    mapM_ 
        (\(out, fs) -> failWith $ "Rebinding of " <> show out <> " at " show fs)
        redefines
  where
    redefines = 
        HM.toList $
        HM.filter ((>1) . length) $
        foldl (\hm (out,f) -> HM.insertWith (++) (unwrapABnd out) [f] hm) HM.empty $
        [(out,f) | LetPureFun f@(PureFun out _ _) c <- universe e] ++
        [(out,f) | LetStateFun f@(StateFun (_,out) _ _ _) c <- universe e] ++
        [(out,f) | LetStateFun f@(StateFun (Just out,_) _ _ _) c <- universe e]

-- | Transform an ALang expression into a DFExpression.
-- This assumes a certain structure in the expression.
-- This can be achieved with the 'normalize' and 'performSSA' functions and tested with
-- 'checkProgramValidity'.
-- lowerALang :: MonadOhua m => Expression -> m NormalizedExpr
-- lowerALang expr
--     -- traceM $ "Lowering alang expr: " <> quickRender expr
--  = do
--     logDebugN $ "Lowering alang expr: " <> quickRender expr
--     (var, exprs) <- runWriterT $ lowerToDF' expr
--     return $ DFExpr exprs var

lowerToDF' :: (MonadOhua m, MonadWriter NormalizedExpr m) => Expression -> m Expression
lowerToDF' = descendM f
    where
        f e@(Var bnd) = tell (pure $ VarFun bnd) >> pure e
        f e@(Let assign expr rest) = do
            descendM f rest
            (app, rest') <- handleDefinitionalExpr' assign expr rest
            case app of 
                PureFun{} -> censor $ pure $ LetPureFun app
                StateFun{} -> censor $ pure $ LetStateFun app
            pure e
        f e = failWith $ "Expected `let` or binding: " <> show e

handleDefinitionalExpr' :: (MonadOhua m)
    => Binding -> Expression -> Expression -> m (App a, Expression)
handleDefinitionalExpr' assign l@(Apply _ _) cont = do
    (fn, s, args) <- handleApplyExpr l
    args' <- mapM expectVar args
    case s of
        Just stateBnd -> 
            return $ 
                first (\outs -> StateFun outs fn (StateBinding stateBnd) (erroringNE args')) $ 
                findSTOuts assign cont
        Nothing -> return (PureFun (DataBinding assign) fn (erroringNE args'), cont)
    where 
        erroringNE = fromMaybe (error "Invariant broken: Every function has at least one argument!") . nonEmpty
        findSTOuts bnd cont = 
            case findDestructuredWithExpr cont bnd of
                [(stateOut, oldStateOut, newStateOut), (dataOut, oldDataOut, newDataOut)] -> 
                    ((Just $ StateBinding stateOut, dataOut)
                    -- this clearly assumes an order! (oldDataOut is contained in newStateOut)
                    , replaceExpr (oldDataOut, newDataOut) $ replaceExpr (oldStateOut, newStateOut) cont)
                _ -> ((Nothing, bnd), cont)
handleDefinitionalExpr' _ e _ =
    failWith $ "Definitional expressions in a let can only be 'apply' but got: " <>
    show e

-- FIXME This function should immediately turn into a pure function once issue #8 is done.
-- | Analyze an apply expression, extracting the inner stateful
-- function and the nested arguments as a list. 
handleApplyExpr ::
       (MonadOhua m)
    => Expression
    -> m (QualifiedBinding, Maybe (ABinding 'State), [Expression])
handleApplyExpr l@(Apply _ _) = go [] l
  where
    go args =
        \case
            Apply fn arg -> go (arg : args) fn
            -- ve@Var {} ->
            --     fromEnv (options . callLocalFunction) >>= \case
            --         Nothing ->
            --             failWith
            --                 "Calling local functions is not supported in this adapter"
            --         Just fn -> return (fn, Nothing, ve : args)
            PureFunction fn _fnId -> return (fn, Nothing, args)
            StatefulFunction fn fnId state -> do
                state' <- expectVar state
                return (fn, Just state', args)
            -- ve@(Lit v) ->
            --     case v of
            --         EnvRefLit _ ->
            --             fromEnv (options . callEnvExpr) >>= \case
            --                 Nothing ->
            --                     failWith
            --                         "Calling environment functions is not supported in this adapter"
            --                 Just fn -> (fn, Nothing, ve : args)
            --         other ->
            --             throwError $
            --             "This literal cannot be used as a function :" <>
            --             show (pretty other)
            x ->
                failWith $ "Expected Apply or Var but got: " <>
                show (x :: Expression)
-- handleApplyExpr (PureFunction fn fnId) =
--     (fn, , Nothing, []) <$> maybe generateId return fnId -- what is this?
handleApplyExpr g = failWith $ "Expected apply but got: " <> show g

-- FIXME This assumption would have been better defined at the type level.
-- | Inspect an expression expecting something which can be captured
-- in a DFVar otherwise throws appropriate errors.
expectVar :: (HasCallStack, MonadError Error m) => Expression -> m DFVar
expectVar (Var bnd) = pure $ DFVar $ DataBinding bnd
-- TODO currently only allowed for the unitFn function
-- expectVar r@PureFunction {} =
--     throwError $
--     "Function references are not yet supported as arguments: " <>
--     show (pretty r)
expectVar (Lit l) = pure $ DFEnvVar l
expectVar a =
    throwErrorS $ "Argument must be local binding or literal, was " <> show a

-- | This is again something that should have been there right at the very beginning.
expectStateBnd :: (HasCallStack, MonadError Error m) => Expression -> m (ABinding 'State)
expectStateBnd (Var bnd) = pure $ StateBinding bnd
expectStateBnd a =
    throwErrorS $ "State argument must be local binding, was " <> show a
