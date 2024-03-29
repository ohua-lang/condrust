{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

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
module Ohua.Core.DFLang.Passes where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
import qualified Data.List.NonEmpty.Extra as NEE
import Ohua.Core.ALang.Lang as ALang
import Ohua.Core.InternalFunctions as IFuns (smapFun,ifFun)
import Ohua.Core.ALang.Util
import Ohua.Core.DFLang.Lang as DFLang hiding (length)
import Ohua.Core.DFLang.Passes.DeadCodeElimination (eliminate)
import Ohua.Core.DFLang.Passes.DispatchInsertion (insertDispatch)
import Ohua.Core.Prelude

runCorePasses :: (MonadOhua m) => NormalizedExpr embExpr ty -> m (NormalizedDFExpr embExpr ty)
runCorePasses = removeNth

finalPasses :: (MonadOhua m) =>  NormalizedDFExpr embExpr ty -> m (NormalizedDFExpr embExpr ty)
finalPasses = insertDispatch >=> eliminate


-- I really should not have to do this in the first place.
-- All transformations that need an Nth node because they introduce functions whose output
-- needs destructuring, should be implemented on top of NormalizedDFExpr.

-- Currently, this code does not cover destructurings of destructurings but this is ok, because
-- we do not create those.
-- NOTE: This code looks a lot like the code that removes state destructuring when lowering ALang into DFLang
removeNth :: forall embExpr ty m. MonadOhua m => NormalizedExpr embExpr ty-> m (NormalizedDFExpr embExpr ty)
removeNth expr = do
  checkSSA expr
  let exp' = evalState (f expr) HM.empty
  return exp'
  where
    -- Assumption: Expression is in SSA form, so every site is uniquely identified by its output binding.
    -- explicit traversal prevents non-exhaustive pattern warnings and allows to convert stuff to Coq later on.
    f = \case
      (DFLang.Let app cont) -> go app cont
      (DFLang.Var atBnd) -> pure $ DFLang.Var atBnd
      where
        go ::
          App a embExpr ty ->
          NormalizedExpr embExpr ty ->
          State
            (HM.HashMap (TypedBinding ty) (NonEmpty (Integer, TypedBinding ty)))
            (NormalizedDFExpr embExpr ty)
        go app cont = do
          cont' <- f cont
          app' <- toDFAppFun app
          pure $ maybe cont' (`DFLang.Let` cont') app'

    -- Note how this makes sure to preserve the semantics of the functions!
    -- TODO we would normally say that the binding does not change its type!
    -- FIXME This only works for destructed App output but what about SMap and Rec?!
    toDFAppFun :: App a embExpr ty
               -> State
               (HM.HashMap (TypedBinding ty) (NonEmpty (Integer, TypedBinding ty)))
               (Maybe (DFApp a embExpr ty))
    toDFAppFun (PureFun tgt (FunRef "ohua.lang/nth" _) (DFEnvVar _ (NumericLit i) :| [_, DFVar srcATB ]) ) =
      modify (HM.insertWith (<>) (unwrapTB srcATB) ((i, unwrapVarTB tgt) :| [])) >> pure Nothing
    toDFAppFun (PureFun out (FunRef fr _) ins) | fr == IFuns.ifFun = do
      hm <- get
      let out' =
            case toDFOuts (unwrapVarTB out) DataBinding hm of
              Destruct (trueOut :| [falseOut]) -> (trueOut, falseOut)
              _ -> error $ "Invariant broken: IfFun has wrong output: " <> show out'
      let dIn = case ins of
                  (d :| []) -> d
                  _ -> error $ "Invariant broken: IfFun has wrong input:" <> show ins
      return $ Just $ IfFun out' dIn
    toDFAppFun (PureFun out (FunRef fr _) ins) | fr == IFuns.smapFun = do
      hm <- get
      let out' =
            case toDFOuts (unwrapVarTB out) DataBinding hm of
              Destruct (dOut :| [ctrlOut, sizeOut]) -> (Just dOut, Just ctrlOut, Just sizeOut)
              _ -> error $ "Invariant broken: SMap has wrong output: " <> show out'
      let dIn = case ins of
                  (d :| []) -> d
                  _ -> error $ "Invariant broken: SMap has wrong input:" <> show ins
      return $ Just $ SMapFun out' dIn
    toDFAppFun (PureFun out fun ins) = do
      hm <- get
      let out' = toDFOuts (unwrapVarTB out) DataBinding hm
      return $ Just $ PureDFFun out' fun ins
    toDFAppFun (StateFun (stateOut, out) fun stateIn ins) = do
      hm <- get
      let out' = toDFOuts (unwrapTB out) DataBinding hm
      let stateOut' = (\s -> toDFOuts (unwrapTB s) StateBinding hm) <$> stateOut
      return $ Just $ StateDFFun (stateOut', Just out') fun stateIn ins
    toDFOuts :: TypedBinding ty -> (TypedBinding ty -> ATypedBinding b ty) -> HM.HashMap (TypedBinding ty) (NonEmpty (Integer, TypedBinding ty)) -> OutData b ty
    toDFOuts out bndFun =
      maybe
        -- TODO normally I would not have to unwrap the bindings here but they would preserve their
        --      annotations and therewith make sure that they do not lose their semantics!
        (Direct $ bndFun out)
        (Destruct . NE.map (Direct . bndFun . snd) . NEE.sortOn fst)
        . HM.lookup out

-- | This pass makes sure no function application is using a binding that has not been defined.
--   TODO: This is once more something that we should enforce via the type system or Liquid Haskell!
checkDefinedUsage :: MonadOhua m => NormalizedDFExpr embExpr ty -> m ()
checkDefinedUsage expr = evalStateT (f expr) HS.empty
  where
    f (DFLang.Let app cont) = checkAndDescend app cont
    f _ = return ()
    checkAndDescend :: (MonadOhua m, MonadState (HS.HashSet (TypedBinding ty)) m) => DFApp a embExpr ty -> NormalizedDFExpr embExpr ty -> m ()
    checkAndDescend app cont = do
      let ins = insDFApp app
      let outs = outsDFApp app
      defined <- get
      mapM_ (failWith . ("Undefined binding:" <>) . show) $
        filter (not . (`HS.member` defined)) ins
      put $ HS.union defined $ HS.fromList outs
      f cont

-- FIXME This thing should be a type level annotation, so we do not have to verify
--       this anymore at runtime! (Use a function to tag an expression with SSA.
--       The tag is lost whenever a new binding is introduced.)

-- | Check that a sequence of let expressions does not redefine bindings.
checkSSA :: forall b embExpr ty m. MonadOhua m => DFLang.Expr b embExpr ty -> m ()
checkSSA e =
  mapM_
    (\(out, fs) -> failWith $ "Rebinding of " <> show out <> " at " <> show fs)
    redefines
  where
    redefines :: [(TypedBinding ty, [Text])]
    redefines =
      HM.toList $
        HM.filter ((> 1) . length) $
          foldl
            (\hm (out, f) -> HM.insertWith (++) out [f] hm)
            HM.empty
            (allOuts e)
    allOuts :: DFLang.Expr b embExpr ty -> [(TypedBinding ty, Text)]
    allOuts (DFLang.Let app cont) = map (,show app) (outBindings app) ++ allOuts cont
    allOuts _ = []

-- | Transform an ALang expression into a DFExpression.
-- This assumes a certain structure in the expression.
-- This can be achieved with the 'normalize' and 'performSSA' functions and tested with
-- 'checkProgramValidity'.
-- lowerALang :: MonadOhua m => Expression -> m (NormalizedExpr embExpr ty)
-- lowerALang expr
--     -- traceM $ "Lowering alang expr: " <> quickRender expr
--  = do
--     logDebugN $ "Lowering alang expr: " <> quickRender expr
--     (var, exprs) <- runWriterT $ lowerToDF' expr
--     return $ DFExpr exprs var
lowerToDF :: MonadOhua m => ALang.Expr embExpr ty -> m (NormalizedExpr embExpr ty)
lowerToDF expr = evalStateT (transfer' expr) HS.empty
  where
    transfer' ::
      (MonadState (HS.HashSet (TypedBinding ty)) m, MonadOhua m) =>
      ALang.Expr embExpr ty ->
      m (NormalizedExpr embExpr ty)
    transfer' (ALang.Var tBnd) = return $ DFLang.Var $ DataBinding tBnd
    transfer' (ALang.Let tBnd a@(NthFunction b) e) = do
      isStateDestruct <- HS.member b <$> get
      if isStateDestruct
        then transfer' e
        else transferLet tBnd a e
    transfer' (ALang.Let tBnd a e) = transferLet tBnd a e
    transfer' e = failWith $ "Invariant broken. Unexpected expression: " <> show e -- FIXME only here because of ALang type (see issue #8)

    transferLet tBnd a e = do
      app <- handleDefinitionalExpr' tBnd a e
      e' <- transfer' e
      return $ app e'

handleDefinitionalExpr' :: forall embExpr ty m.
  (MonadState (HS.HashSet (TypedBinding ty) ) m, MonadOhua m) =>
  TypedBinding ty ->
  ALang.Expr embExpr ty ->
  ALang.Expr embExpr ty ->
  m (NormalizedExpr embExpr ty -> NormalizedExpr embExpr ty)
handleDefinitionalExpr' assign l@(Apply _ _) cont = do
  (fn, s, args) <- handleApplyExpr l
  args' <- mapM (uncurry expectVar) args
  case s of
    Just stateBnd -> DFLang.Let <$> st fn stateBnd args'
    Nothing -> return $ DFLang.Let $ fun fn assign args'
  where
    st ::
      (MonadState (HS.HashSet (TypedBinding ty)) m) =>
      FunRef ty Resolved ->
      ATypedBinding 'State ty ->
      NonEmpty (DFVar 'Data embExpr ty) ->
      m (App 'ST embExpr ty)
    st fn stateATBnd args' =
      (\outs -> StateFun outs fn (DFVar stateATBnd) args')
        <$> findSTOuts assign
    fun :: FunRef ty Resolved -> TypedBinding ty -> NonEmpty (DFVar 'Data embExpr ty) -> App 'Fun embExpr ty
    fun fn tbnd args' = PureFun (DFVar $ DataBinding tbnd) fn args'
    findSTOuts ::
      (MonadState (HS.HashSet (TypedBinding ty)) m) =>
      TypedBinding ty ->
      m (Maybe (ATypedBinding 'State ty), ATypedBinding 'Data ty)
    findSTOuts bnd =
      case findDestructured cont bnd of
        [stateOut, dataOut] -> do
          modify (HS.insert bnd)
          return (Just $ StateBinding stateOut, DataBinding dataOut)
        _ -> return (Nothing, DataBinding bnd)
handleDefinitionalExpr' _ e _ =
  failWith $
    "Definitional expressions in a let can only be 'apply' but got: "
      <> show e

-- FIXME This function should immediately turn into a pure function once issue #8 is done.

-- | Analyze an apply expression, extracting the inner stateful
-- function and the nested arguments as a list.
--Question: I've noticed (through error messages, that this isn't just called with stateful functions. e.g. in the testcase
{-                use crate::funs::*;

                fn rec(one:i32, two:i32) -> i32 {
                    let i:i32 = h(one);
                    let j:i32 = h(two);
                    let k:i32 = h4(i, j);
                    if check(k) {
                        rec(i,j)FunRef ty  Resolved
                    } else {
                        k
                    }
                }

                fn test() -> i32 {
                    rec(2,4)
                }
    (At least) the function h4 is also processed here. So is it supposed to only process stateful calls or is the error message wrong?
-}

handleApplyExpr ::
  forall m embExpr ty.
  (MonadOhua m) =>
  ALang.Expr embExpr ty ->
  m (FunRef ty Resolved, Maybe (ATypedBinding 'State ty), NonEmpty (OhuaType ty Resolved, ALang.Expr embExpr ty))
handleApplyExpr (Apply fn a) = go (a :| []) fn
  where
    go args e =
      case e of
        Apply f arg -> do
          go (arg NE.<| args) f
        Lit (FunRefLit fr@(FunRef f  (FunType argTypes retTy))) -> do
          let expInpTypes = expectedInputTypesResolved argTypes
          assertTermTypes args expInpTypes "function" f
          return (fr, Nothing, NE.zip expInpTypes args)
        Lit (FunRefLit (FunRef qb STFunType {})) ->
          failWith $ "Wrong function type 'st' for pure function: " <> show qb
        BindState _state0 (Lit (FunRefLit (FunRef f FunType {}))) ->
          failWith $ "Wrong function type 'pure' for st function: " <> show f
        BindState state0 (Lit (FunRefLit fr@(FunRef f (STFunType sType argTypes retTy)))) -> do
          let expInpTypes = expectedInputTypesResolved argTypes
          assertTermTypes args expInpTypes "stateful function" f
          state' <- expectStateBnd state0
          return (fr, Just state', NE.zip expInpTypes args)
        x -> failWith $ "Expected Apply or Var but got: " <> show (x :: ALang.Expr embExpr ty)
    assertTermTypes termArgs typeArgs funType f =
      assertE
        (length termArgs == length' typeArgs)
        $ "Arg types [len: "
          <> show (length typeArgs)
          <> " types: " <> foldl (\s t -> s <>", " <> show t) "" typeArgs
          <> "] and args [len: "
          <> show (length termArgs)
          <> " terms: " <> foldl (\s t -> s <>", " <> show t) "" termArgs
          <> "] don't match for stateful "
          <> funType
          <> ": "
          <> show f

    -- The compiler adds a Unit term to the terms of an application when it has actually no arguments
    -- so in that case, we expect the terms list to be one item longer than the list of argument types
    -- This modification of length fixes the comparison (in a quity hacky way :-/)
    length' types = if null types
                      then 1
                      else length types
  
    zip' :: [OhuaType ty Resolved] -> NonEmpty b -> NonEmpty (OhuaType ty Resolved, b)
    zip' [] bs = NE.zip (IType TypeUnit :| []) bs
    zip' (a:as) bs = NE.zip (a:|as) bs
handleApplyExpr g = failWith $ "Expected apply but got: " <> show g

-- FIXME This assumption would have been better defined at the type level.

-- | Inspect an expression expecting something which can be captured
-- in a DFVar otherwise throws appropriate errors.
-- ToDo: This should go away because a) We carry the types of vars (and literals?!) from the frontend and b) at this point there shouldn't be syntax errors any more
expectVar :: (HasCallStack, MonadError Error m) => OhuaType ty Resolved -> ALang.Expr embExpr ty -> m (DFVar 'Data embExpr ty)
expectVar _ (ALang.Var tBnd) = pure $ DFVar $ DataBinding tBnd
-- TODO currently only allowed for the unitFn function
-- expectVar r@PureFunction {} =
--     throwError $
--     "Function references are not yet supported as arguments: " <>
--     show (pretty r)
expectVar _ (Lit l) = pure $ DFEnvVar (getLitType l) l
expectVar _ a =
  throwErrorS $ "Argument must be local binding or literal, was " <> show a

-- | This is again something that should have been there right at the very beginning.
expectStateBnd :: (HasCallStack, MonadError Error m) => ALang.Expr embExpr ty -> m (ATypedBinding 'State ty)
expectStateBnd (ALang.Var tBnd) = pure $ StateBinding tBnd
expectStateBnd a =
  throwErrorS $ "State argument must be local binding, was " <> show a

