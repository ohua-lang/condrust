{-# LANGUAGE ScopedTypeVariables #-}

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
import Ohua.Core.ALang.Refs as ALangRefs (smapFun,ifFun)
import Ohua.Core.ALang.Util
import Ohua.Core.DFLang.Lang as DFLang hiding (length)
import Ohua.Core.DFLang.Passes.DeadCodeElimination (eliminate)
import Ohua.Core.DFLang.Passes.DispatchInsertion (insertDispatch)
import Ohua.Core.DFLang.Passes.TypePropagation (propagateTypesWithRetTy)
import Ohua.Core.Prelude

runCorePasses :: (MonadOhua m) => NormalizedExpr ty -> m (NormalizedDFExpr ty)
runCorePasses = removeNth

finalPasses :: (MonadOhua m) =>  NormalizedDFExpr ty -> m (NormalizedDFExpr ty)
finalPasses = insertDispatch >=> eliminate
 
typePropagation :: (MonadOhua m) => ty -> NormalizedDFExpr ty -> m (NormalizedDFExpr ty)
typePropagation retTy = pure . propagateTypesWithRetTy retTy

-- I really should not have to do this in the first place.
-- All transformations that need an Nth node because they introduce functions whose output
-- needs destructuring, should be implemented on top of NormalizedDFExpr.

-- Currently, this code does not cover destructurings of destructurings but this is ok, because
-- we do not create those.
-- NOTE: This code looks a lot like the code that removes state destructuring when lowering ALang into DFLang
removeNth :: forall ty m. MonadOhua m => NormalizedExpr ty-> m (NormalizedDFExpr ty)
removeNth expr = do
  checkSSA expr
  let exp' = evalState (f expr) HM.empty
  return exp'
  where
    -- Assumption: Expression is in SSA form, so every site is uniquely identified by its output binding.

    -- explicit traversal prevents non-exhaustive pattern warnings and allows to convert stuff to Coq later on.
    f = \case
      (DFLang.Let app cont) -> go app cont
      (DFLang.Var bnd ty) -> pure $ DFLang.Var bnd ty
      where
        go ::
          App a ty ->
          NormalizedExpr ty ->
          State
            (HM.HashMap Binding (NonEmpty (Integer, Binding)))
            (NormalizedDFExpr ty)
        go app cont = do
          cont' <- f cont
          app' <- toDFAppFun app
          pure $ maybe cont' (`DFLang.Let` cont') app'

    -- Note how this makes sure to preserve the semantics of the functions!
    -- TODO we would normally say that the binding does not change its type!
    -- FIXME This only works for destructed App output but what about SMap and Rec?!
    toDFAppFun :: App a ty
               -> State
               (HM.HashMap Binding (NonEmpty (Integer, Binding)))
               (Maybe (DFApp a ty))
    toDFAppFun (PureFun tgt (FunRef "ohua.lang/nth" _ _) (DFEnvVar _ (NumericLit i) :| [_, DFVar _ (DataBinding src)]) ) =
      modify (HM.insertWith (<>) src ((i, unwrapABnd tgt) :| [])) >> pure Nothing
    toDFAppFun (PureFun out (FunRef fr _ _) ins) | fr == ALangRefs.ifFun = do
      hm <- get
      let out' =
            case toDFOuts (unwrapABnd out) DataBinding hm of
              Destruct (trueOut :| [falseOut]) -> (trueOut, falseOut)
              _ -> error $ "Invariant broken: IfFun has wrong output: " <> show out'
      let dIn = case ins of
                  (d :| []) -> d
                  _ -> error $ "Invariant broken: IfFun has wrong input:" <> show ins
      return $ Just $ IfFun out' dIn
    toDFAppFun (PureFun out (FunRef fr _ _) ins) | fr == ALangRefs.smapFun = do
      hm <- get
      let out' =
            case toDFOuts (unwrapABnd out) DataBinding hm of
              Destruct (dOut :| [ctrlOut, sizeOut]) -> (Just dOut, Just ctrlOut, Just sizeOut)
              _ -> error $ "Invariant broken: SMap has wrong output: " <> show out'
      let dIn = case ins of
                  (d :| []) -> d
                  _ -> error $ "Invariant broken: SMap has wrong input:" <> show ins
      return $ Just $ SMapFun out' dIn
    toDFAppFun (PureFun out fun ins) = do
      hm <- get
      let out' = toDFOuts (unwrapABnd out) DataBinding hm
      return $ Just $ PureDFFun out' fun ins
    toDFAppFun (StateFun (stateOut, out) fun stateIn ins) = do
      hm <- get
      let out' = toDFOuts (unwrapABnd out) DataBinding hm
      let stateOut' = (\s -> toDFOuts (unwrapABnd s) StateBinding hm) <$> stateOut
      return $ Just $ StateDFFun (stateOut', Just out') fun stateIn ins
    toDFOuts :: Binding -> (Binding -> ABinding a) -> HM.HashMap Binding (NonEmpty (Integer, Binding)) -> OutData a
    toDFOuts out bndFun =
      maybe
        -- TODO normally I would not have to unwrap the bindings here but they would preserve their
        --      annotations and therewith make sure that they do not lose their semantics!
        (Direct $ bndFun out)
        (Destruct . NE.map (Direct . bndFun . snd) . NEE.sortOn fst)
        . HM.lookup out

-- | This pass makes sure no function application is using a binding that has not been defined.
--   TODO: This is once more something that we should enforce via the type system or Liquid Haskell!
checkDefinedUsage :: MonadOhua m => NormalizedDFExpr ty -> m ()
checkDefinedUsage expr = evalStateT (f expr) HS.empty
  where
    f (DFLang.Let app cont) = checkAndDescend app cont
    f _ = return ()
    checkAndDescend :: (MonadOhua m, MonadState (HS.HashSet Binding) m) => DFApp a ty -> NormalizedDFExpr ty -> m ()
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
checkSSA :: forall ty a m. MonadOhua m => DFLang.Expr ty a -> m ()
checkSSA e =
  mapM_
    (\(out, fs) -> failWith $ "Rebinding of " <> show out <> " at " <> show fs)
    redefines
  where
    redefines :: [(Binding, [Text])]
    redefines =
      HM.toList $
        HM.filter ((> 1) . length) $
          foldl
            (\hm (out, f) -> HM.insertWith (++) out [f] hm)
            HM.empty
            (allOuts e)
    allOuts :: DFLang.Expr ty a -> [(Binding, Text)]
    allOuts (DFLang.Let app cont) = (map (,show app) (outBindings app)) ++ allOuts cont
    allOuts _ = []

-- | Transform an ALang expression into a DFExpression.
-- This assumes a certain structure in the expression.
-- This can be achieved with the 'normalize' and 'performSSA' functions and tested with
-- 'checkProgramValidity'.
-- lowerALang :: MonadOhua m => Expression -> m (NormalizedExpr ty)
-- lowerALang expr
--     -- traceM $ "Lowering alang expr: " <> quickRender expr
--  = do
--     logDebugN $ "Lowering alang expr: " <> quickRender expr
--     (var, exprs) <- runWriterT $ lowerToDF' expr
--     return $ DFExpr exprs var
lowerToDF :: MonadOhua m => ALang.Expr ty -> m (NormalizedExpr ty)
lowerToDF expr = evalStateT (transfer' expr) HS.empty
  where
    transfer' ::
      (MonadState (HS.HashSet Binding) m, MonadOhua m) =>
      ALang.Expr ty ->
      m (NormalizedExpr ty)
    transfer' (ALang.Var bnd) = return $ DFLang.Var bnd TypeVar
    transfer' (ALang.Let bnd a@(NthFunction b) e) = do
      isStateDestruct <- HS.member b <$> get
      if isStateDestruct
        then transfer' e
        else transferLet bnd a e
    transfer' (ALang.Let bnd a e) = transferLet bnd a e
    transfer' e = failWith $ "Invariant broken. Unexpected expression: " <> show e -- FIXME only here because of ALang type (see issue #8)
    transferLet bnd a e = do
      app <- handleDefinitionalExpr' bnd a e
      e' <- transfer' e
      return $ app e'

handleDefinitionalExpr' ::
  (MonadState (HS.HashSet Binding) m, MonadOhua m) =>
  Binding ->
  ALang.Expr ty ->
  ALang.Expr ty ->
  m (NormalizedExpr ty -> NormalizedExpr ty)
handleDefinitionalExpr' assign l@(Apply _ _) cont = do
  (fn, s, args) <- handleApplyExpr l
  args' <- mapM (uncurry expectVar) args
  case s of
    Just stateBnd -> DFLang.Let <$> st fn stateBnd args'
    Nothing -> return $ DFLang.Let $ fun fn assign args'
  where
    st ::
      (MonadState (HS.HashSet Binding) m) =>
      FunRef ty ->
      (ArgType ty, ABinding 'State) ->
      NonEmpty (DFVar 'Data ty) ->
      m (App 'ST ty)
    st fn (stateType, stateBnd) args' =
      (\outs -> StateFun outs fn (DFVar stateType stateBnd) args')
        <$> findSTOuts assign
    fun :: FunRef ty -> Binding -> NonEmpty (DFVar 'Data ty) -> App 'Fun ty
    fun fn bnd args' = PureFun (DataBinding bnd) fn args'
    findSTOuts ::
      (MonadState (HS.HashSet Binding) m) =>
      Binding ->
      m (Maybe (ABinding 'State), ABinding 'Data)
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
handleApplyExpr ::
  forall m ty.
  (MonadOhua m) =>
  ALang.Expr ty ->
  m (FunRef ty, Maybe (ArgType ty, ABinding 'State), NonEmpty (ArgType ty, ALang.Expr ty))
handleApplyExpr (Apply fn a) = go (a :| []) fn
  where
    go args e =
      case e of
        Apply f arg -> do
          go (arg NE.<| args) f
        Lit (FunRefLit fr@(FunRef f _ident (FunType argTypes))) -> do
          assertTermTypes args argTypes "function" f
          return (fr, Nothing, zip' argTypes args)
        Lit (FunRefLit (FunRef qb _ Untyped)) ->
          failWith $ "Wrong function type 'untyped' for pure function: " <> show qb
        Lit (FunRefLit (FunRef qb _ STFunType {})) ->
          failWith $ "Wrong function type 'st' for pure function: " <> show qb
        BindState _state0 (Lit (FunRefLit (FunRef f _ Untyped))) ->
          failWith $ "Wrong function type 'untyped' for st function: " <> show f
        BindState _state0 (Lit (FunRefLit (FunRef f _ FunType {}))) ->
          failWith $ "Wrong function type 'pure' for st function: " <> show f
        BindState state0 (Lit (FunRefLit fr@(FunRef f _ (STFunType sType argTypes)))) -> do
          assertTermTypes args argTypes "stateful function" f
          state' <- expectStateBnd state0
          return (fr, Just (sType, state'), zip' argTypes args)
        x -> failWith $ "Expected Apply or Var but got: " <> show (x :: ALang.Expr ty)
    assertTermTypes termArgs typeArgs funType f =
      assertE
        -- length of Unit = 1?
        -- implement Foldable for Either (Unit (NonEmpty ...))
        (length termArgs == length' typeArgs)
        $ "Arg types [len: " <> show (length' typeArgs)
          <> "] and args [len: "
          <> show (length termArgs)
          <> "] don't match for stateful "
          <> funType
          <> ": "
          <> show f
    length' expr = case expr of
      Left Unit -> 1
      Right l -> length l

    zip' :: Either Unit (NonEmpty (ArgType ty)) -> NonEmpty b -> NonEmpty (ArgType ty, b)
    zip' (Left Unit) bs = NE.zip (TypeVar :| []) bs -- FIXME this seems wrong to me. it should be a unitVal. It points to a problem that we we still have with Unit. We do not distinguish between a unit type and a unit value. and I'm not even sure that there should be such a thing as a unit value unless the controls need it.
    zip' (Right as) bs = NE.zip as bs
handleApplyExpr g = failWith $ "Expected apply but got: " <> show g

-- FIXME This assumption would have been better defined at the type level.

-- | Inspect an expression expecting something which can be captured
-- in a DFVar otherwise throws appropriate errors.
expectVar :: (HasCallStack, MonadError Error m) => ArgType ty -> ALang.Expr ty -> m (DFVar 'Data ty)
expectVar typ (ALang.Var bnd) = pure $ DFVar typ $ DataBinding bnd
-- TODO currently only allowed for the unitFn function
-- expectVar r@PureFunction {} =
--     throwError $
--     "Function references are not yet supported as arguments: " <>
--     show (pretty r)
expectVar typ (Lit l) = pure $ DFEnvVar typ l
expectVar _ a =
  throwErrorS $ "Argument must be local binding or literal, was " <> show a

-- | This is again something that should have been there right at the very beginning.
expectStateBnd :: (HasCallStack, MonadError Error m) => ALang.Expr ty -> m (ABinding 'State)
expectStateBnd (ALang.Var bnd) = pure $ StateBinding bnd
expectStateBnd a =
  throwErrorS $ "State argument must be local binding, was " <> show a
