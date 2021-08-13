module Ohua.Core.DFLang.Passes.TypePropagation where

import Data.HashMap.Lazy as HM
import Ohua.Core.DFLang.Lang
import qualified Ohua.Core.DFLang.Refs as Refs
import Ohua.Core.Prelude
import qualified Ohua.Types.Vector as OV

import qualified Data.List.NonEmpty as NE

data Exists ty = forall semTy.Exists (DFVar semTy ty)

-- TODO collect variables from all calls (pure and stateful)
-- TODO handle smapFun, recurFun, select, ifFun, ....
propagateTypes :: NormalizedDFExpr ty -> NormalizedDFExpr ty
propagateTypes e = evalState (transformExprM go e) HM.empty
  where
    go ::
      NormalizedDFExpr ty ->
      State (HM.HashMap Binding (Exists ty)) (NormalizedDFExpr ty)
    -- (Sebastian to himself): Implement these damn types already!!!
    go (Let (PureDFFun out@(Direct outBnd) f@(FunRef fun _ _) [DFVar cty ctrlSig, DFVar ty dataInpBnd]) ct)
      | fun == Refs.ctrl = do
        traceM $ "Looking at ctrl control input type " <> show cty
        traceM $ "Looking at ctrl input type " <> show ty
        vars <- get
        let dataInp' = case HM.lookup (unwrapABnd outBnd) vars of
              Just (Exists (DFVar ty' _)) -> DFVar ty' dataInpBnd
              _ -> DFVar ty dataInpBnd
        -- add it to the used vars
        modify (HM.insert (unwrapABnd dataInpBnd) $ Exists dataInp')

        -- FIXME: THIS IS ONE BIG HACK. Better solution would be pulling information from the function signature
        let ctrlInp' = case cty of
              TypeVar -> DFVar (TupleTy (TypeVar :| [TypeVar])) ctrlSig
              _ -> DFVar cty ctrlSig
        modify (HM.insert (unwrapABnd ctrlSig) $ Exists ctrlInp')

        return $ Let (PureDFFun out f [ctrlInp', dataInp']) ct
    go (Let (PureDFFun out@(Destruct ((Direct dataOut) :| outBnds)) f@(FunRef fun _ _) [DFVar ty dataInpBnd]) ct)
      | fun == Refs.smapFun = do
          -- Assumption: The first item in the list is always the data out!
          traceM $ "Looking at smap input type " <> show ty
          vars <- get
          let dataInp' = case HM.lookup (unwrapABnd dataOut) vars of
                Just (Exists (DFVar ty' _)) -> DFVar ty' dataInpBnd
                _ -> DFVar ty dataInpBnd
          modify (HM.insert (unwrapABnd dataInpBnd) $ Exists dataInp')
          return $ Let (PureDFFun out f [dataInp']) ct
    go (Let (PureDFFun out f@(FunRef fun _ (FunType (Right lst))) vars) ct) = do
      traceM $ "Checking " <> show fun
      -- vars <- get
      let dataInps' = NE.map (\case
                                 (DFVar _ bnd, ty') -> DFVar ty' bnd
                                 (DFEnvVar _ lit, ty') -> DFEnvVar ty' lit
                             ) $ NE.zip vars lst
      -- traceM $ "Running this bad boi on " <> show fun
      mapM_ (\case
               v@(DFVar _ bnd) -> modify (HM.insert (unwrapABnd bnd) $ Exists v)
               _ -> return ()
           ) dataInps'
      return $ Let (PureDFFun out f dataInps') ct

    -- Stateful Functions
    go (Let (StateDFFun outBnds f@(FunRef _ _ (STFunType sty tyInfo)) stateIn dataIn) ct) = do
      let stateIn' = case stateIn of
            (DFVar ty bnd) -> DFVar (pickType ty sty) bnd
            v@DFEnvVar{} -> v
      let dataIn' = case tyInfo of
            (Right info) -> NE.map (\case
                                       (DFVar _ bnd, ty') -> DFVar ty' bnd
                                       (DFEnvVar _ lit, ty') -> DFEnvVar ty' lit
                                   ) $ NE.zip dataIn info
            (Left Unit) -> dataIn
      -- update state
      case stateIn' of
        v@(DFVar _ bnd) -> modify (HM.insert (unwrapABnd bnd) $ Exists v)
        _ -> return ()
      mapM_ (\case
                v@(DFVar _ bnd) -> modify (HM.insert (unwrapABnd bnd) $ Exists v)
                _ -> return ()
            ) dataIn'
      return $ Let (StateDFFun outBnds f stateIn' dataIn') ct

    -- Recursion
    go (Let (RecurFun finalOut recCtrl argOuts initIns recIns cond result) ct) = do
      -- TODO: Try to involve type info from the argOuts??
      let (initIns', recIns') = OV.unzip $ OV.map updateVars $ OV.zip initIns recIns
      -- add new types to the pool
      mapM_ (\case
               v@(DFVar _ bnd) -> modify (HM.insert (unwrapABnd bnd) $ Exists v)
               _ -> return ()
           ) $ OV.toList initIns' ++ OV.toList recIns'
      return $ Let (RecurFun finalOut recCtrl argOuts initIns' recIns' cond result) ct
    go e' = return e'

    -- | Updates the type signatures of two zipped DFVars to match them, giving the EnvVar type preference over the (possibly) inferred type of the other DFVar
    updateVars :: (DFVar a ty, DFVar a ty) -> (DFVar a ty, DFVar a ty)
    updateVars (DFVar t bnd, DFVar t2 bnd2) =
      let newTy = pickType t t2
      in (DFVar newTy bnd, DFVar newTy bnd2)
    updateVars (DFVar t bnd, DFEnvVar t2 lit) =
      -- this case should not occur in recurFun and is only here for the sake of completeness
      let newTy = pickType t2 t
      in (DFVar newTy bnd, DFEnvVar newTy lit)
    updateVars (DFEnvVar t lit, DFVar t2 bnd2) =
      let newTy = pickType t t2
      in (DFEnvVar newTy lit, DFVar newTy bnd2)
    updateVars (v1@DFEnvVar{}, v2@DFEnvVar{}) = (v1, v2)

    -- | Picks a Argtype from two available ones, choosing any type over TypeVar or the first one.
    pickType :: ArgType ty -> ArgType ty -> ArgType ty
    pickType TypeVar TypeVar = TypeVar
    pickType t TypeVar = t
    pickType TypeVar t = t
    pickType t _ = t
