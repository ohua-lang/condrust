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
propagateTypes e = evalState (transformExprM typeBottomUp e) HM.empty
-- What we know are the input types of functions parsed in the frontend
-- Those types contain the types of parameters only i.e. no return type !
-- 1. This means we can start bottom up. When we find a function call that is typed, we can assign types to the
--      bindings/variables passed to that function. Those variable names are bound somewhere in the outer scope so we can
--      check for every `let out = something`  if `out` was the input of a function in the inner scope and is therefore already
--      typed
-- 2. On the other hand, we have Ohua inserted functions, for which we don't know the input types. For these functions we
--    need a second top-down pass ... We already know the output types of funtions from the pass before and therefore can type 
--    the inputs of these functions, given the typed variables entering them. 
-- Ohua Inserted functions are:
{-
  Refs.ctrl   -> Type should be: (bool, A) -> A
  Refs.select -> Type should be; (bool, A, A) -> A
  Refs.ifFun  -> Type should be: (A) -> (bool, bool)
  Refs.collect-> Type should be (int, A) -> [A]
  Refs.seqFun -> Type should be ? => grep doesn't show me any place it's used in the code
              -> looks like: (A, int) -> A ?
  Refs.unitFun-> Type should be: (F, Unit) -> F(), 
              -> i.e. F is a stateles function that was called without arguments,
              -> I don't think we can learn from that since we don't know the return type of
              -> F() as we do not safe the return type of functions (for whatever reason)
              -> In theory we can know the output type here, if we know the output type of F()
              -> this might be the case for e.g. unitFUn(id, lA), which actually occures in the
               respective stage of DFLang. However we'd neeed to identify by adding the F functions
              -> to scope as simple variables (which they actually are.)
  Refs.runSTCLangSMap   -> Type should be : (int, A) -> (A)
  Refs.runSTCLangIf     ->  ??? from Alang.State it seems to take a cond (bool?) and two A (Somestate?) and returns a A (Somestate?)
                        -> Not sure, not sure if it arrives at DFLAng-Custom, not sure what all the "FIXME"s want to 
                        -> tell me about the reanson behind making any ccode depending on this implementation. :-(
  Refs.id     -> Type should be:  A-> A 

-}
  where
    typeBottomUp ::
      NormalizedDFExpr ty ->
      State (HM.HashMap Binding (Exists ty)) (NormalizedDFExpr ty)
    -- (Sebastian to himself): Implement these damn types already!!!
    -- REMINDER: 
      -- fTy : contains ArgType = TypeVar | Type ty | TupleTy (NonEmpty (ArgType ty)) if function is typed
      -- vars: are data DFVar = 
        --                DFEnvVar :: ArgType ty -> Lit ty -> DFVar 'Data ty
        --                DFVar :: ArgType ty -> ABinding a -> DFVar a ty

    typeBottomUp pf@(Let (PureDFFun out@(Direct outBnd) f@(FunRef fun _ _) (DFVar cty ctrlSig :| [DFVar ty dataInpBnd])) inCont)
      -- In this case, the function is an Ohua control node. Those nodes allways take two inputs
      -- a variable and a signal and outputs the variable based on the signal.
      -- So if we know it's input type we also know it's output and the other way around
      -- In a bottom up pass, we've seen the function using the output already.
      -- So we can try to type the input, using the output. 
      | fun == Refs.ctrl = do
        traceM  $ "Typing controle function " <> show fun
        traceM $ "Controle type is " <> show cty
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

        return $ Let (PureDFFun out f (ctrlInp' :| [dataInp'])) inCont
    
    typeBottomUp (Let (PureDFFun out f@(FunRef _fun _ (FunType (Right lst))) vars) inCont) = do
      -- traceM $ "Checking " <> show fun
      -- vars <- get
      let dataInps' = NE.map (\case
                                 (DFVar _ bnd, ty') -> trace ("Found a typed variable "<> show bnd <>" Corresponding input type is " <> show ty') DFVar ty' bnd
                                 (DFEnvVar _ lit, ty') -> DFEnvVar ty' lit
                             ) $ NE.zip vars lst
      -- traceM $ "Running this bad boi on " <> show _fun
      mapM_ (\case
               v@(DFVar _ bnd) -> modify (HM.insert (unwrapABnd bnd) $ Exists v)
               _ -> return ()
           ) dataInps'
      return $ Let (PureDFFun out f dataInps') inCont
    
    typeBottomUp e'@(Let (PureDFFun out (FunRef bnd _ funType) params) inCont) = do
      -- traceM $ "Not typing pure function: " <> show bnd
      -- traceM $ "having type: " <> show funType
      -- traceM $ "and Params: " <> show params
      return e'

    -- SMap
    typeBottomUp (Let (SMapFun out@(Just (Direct dOut), _, _) (DFVar ty dataInpBnd)) inCont) = do
          -- traceM $ "Looking at smap input type " <> show ty
          vars <- get
          let dataInp' = case HM.lookup (unwrapABnd dOut) vars of
                Just (Exists (DFVar ty' _)) -> DFVar ty' dataInpBnd
                _ -> DFVar ty dataInpBnd
          modify (HM.insert (unwrapABnd dataInpBnd) $ Exists dataInp')
          return $ Let (SMapFun out dataInp') inCont
    
    typeBottomUp e'@(Let (SMapFun (outData, outControle, outSize) inp) inCont) = do
      -- traceM $ "Not typing SMap function" <> show e'
      return e'

    -- Stateful Functions
    typeBottomUp (Let (StateDFFun oBnds f@(FunRef _ _ (STFunType sty tyInfo)) stateIn dataIn) inCont) = do
      let stateIn' = case stateIn of
            (DFVar ty bnd) -> DFVar (maxType ty sty) bnd
      let dataIn' = case tyInfo of
            (Right info) -> NE.map (\case
                                       (DFVar _ bnd, ty') -> DFVar ty' bnd
                                       (DFEnvVar _ lit, ty') -> DFEnvVar ty' lit
                                   ) $ NE.zip dataIn info
            (Left Unit) -> dataIn
      -- update state
      case stateIn' of
        v@(DFVar _ bnd) -> modify (HM.insert (unwrapABnd bnd) $ Exists v)
      mapM_ (\case
                v@(DFVar _ bnd) -> modify (HM.insert (unwrapABnd bnd) $ Exists v)
                _ -> return ()
            ) dataIn'
      return $ Let (StateDFFun oBnds f stateIn' dataIn') inCont

    typeBottomUp e'@(Let (StateDFFun oBnds stFun stateIn dataIn) inCont)  = do
      -- traceM $ "Not typing StateDF function" <> show e'
      return e'
     
    -- Recursion
    typeBottomUp (Let (RecurFun finalOut recCtrl argOuts initIns recIns cond result) inCont) = do
      -- TODO: Try to involve type info from the argOuts??
      let (initIns', recIns') = OV.unzip $ OV.map updateVars $ OV.zip initIns recIns
      -- add new types to the pool
      mapM_ (\case
               v@(DFVar _ bnd) -> modify (HM.insert (unwrapABnd bnd) $ Exists v)
               _ -> return ()
           ) $ OV.toList initIns' ++ OV.toList recIns'
      return $ Let (RecurFun finalOut recCtrl argOuts initIns' recIns' cond result) inCont

    typeBottomUp e'@(Let (IfFun _ _ ) _) = do
      traceM $ "Not typing If function" <> show e'
      return e'
    typeBottomUp e'@(Var _ ) = do
      traceM $ "Not typing Var" <> show e'

      return e'

     



    -- | Updates the type signatures of two zipped DFVars to match them, 
    --   giving the EnvVar type preference over the (possibly) inferred type of the other DFVar
    updateVars :: (DFVar a ty, DFVar a ty) -> (DFVar a ty, DFVar a ty)
    updateVars (DFVar t bnd, DFVar t2 bnd2) =
      let newTy = maxType t t2
      in (DFVar newTy bnd, DFVar newTy bnd2)
    updateVars (DFVar t bnd, DFEnvVar t2 lit) =
      -- this case should not occur in recurFun and is only here for the sake of completeness
      let newTy = maxType t2 t
      in (DFVar newTy bnd, DFEnvVar newTy lit)
    updateVars (DFEnvVar t lit, DFVar t2 bnd2) =
      let newTy = maxType t t2
      in (DFEnvVar newTy lit, DFVar newTy bnd2)
    updateVars (v1@DFEnvVar{}, v2@DFEnvVar{}) = (v1, v2)

    -- | Picks a Argtype from two available ones, choosing any type over TypeVar or the first one.
    maxType :: ArgType ty -> ArgType ty -> ArgType ty
    maxType TypeVar TypeVar = TypeVar
    maxType t TypeVar = t
    maxType TypeVar t = t
    maxType t _ = t

