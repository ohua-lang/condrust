module Ohua.Core.DFLang.Passes.TypePropagation where

import Data.HashMap.Lazy as HM hiding (map, foldl')
import Ohua.Core.DFLang.Lang hiding (length)
import qualified Ohua.Core.DFLang.Refs as Refs
import Ohua.Core.Prelude 
import qualified Ohua.Types.Vector as OV

import qualified Data.List.NonEmpty as NE

data Exists ty = forall semTy.Exists (DFVar semTy ty)
type BindingContext ty = (HM.HashMap Binding (Exists ty)) 

controlSignalType :: ArgType ty
controlSignalType = TupleTy $ TypeBool:| [TypeNat]

returnBinding:: Binding
returnBinding = "finalReturnType"

pattern RetBnd :: Binding
pattern RetBnd <-  "finalReturnType"

-- TODO collect variables from all calls (pure and stateful)
-- TODO handle smapFun, recurFun, select, ifFun, ....
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
  Note: The tuple (bool, nat is a control signal)
  Refs.ctrl   -> Type should be: ((bool, nat), A) -> A
  Refs.select -> Type should be; (bool, A, A) -> A
  Refs.ifFun  -> Type should be: (A) -> ((bool, nat),(bool,nat))
  Refs.collect-> Type should be (nat, A) -> [A]
  Refs.seqFun -> Type should be ? => grep doesn't show me any place it's used in the code
              -> looks like: (A, nat) -> A ?
  Refs.unitFun-> Type should be: (F, Unit) -> F(), 
              -> i.e. F is a stateles function that was called without arguments,
              -> I don't think we can learn from that since we don't know the return type of
              -> F() as we do not safe the return type of functions (for whatever reason)
              -> In theory we can know the output type here, if we know the output type of F()
              -> this might be the case for e.g. unitFUn(id, lA), which actually occures in the
               respective stage of DFLang. However we'd neeed to identify by adding the F functions
              -> to scope as simple variables (which they actually are.)
  Refs.runSTCLangSMap   -> Type should be : (nat, A) -> (A)
  Refs.runSTCLangIf     ->  ??? from Alang.State it seems to take a cond (bool?) and two A (Somestate?) and returns a A (Somestate?)
                        -> Not sure, not sure if it arrives at DFLAng-Custom, not sure what all the "FIXME"s want to 
                        -> tell me about the reanson behind making any ccode depending on this implementation. :-(
  Refs.id     -> Type should be:  A -> A 

-}
propagateTypes :: NormalizedDFExpr ty -> NormalizedDFExpr ty
propagateTypes e = evalState (transformExprM typeBottomUp >=> transformExprTDM typeTopDown $ e) HM.empty



propagateTypesWithRetTy :: ty -> NormalizedDFExpr ty -> NormalizedDFExpr ty
propagateTypesWithRetTy retType expr  = 
  -- ToDo: It would be better to pass in the return type and set this Value when we encounter the
  -- first (and only) Expression, that is a Variable and therefor the return value.
  let context = HM.insert returnBinding (Exists$ DFVar (Type retType) (DataBinding returnBinding)) HM.empty 
  in evalState (transformExprM typeBottomUp expr)  context


-- FIXME: This whole function is full of repeated paaterns that need to be extracted !
typeBottomUp ::
  NormalizedDFExpr ty ->
  State (BindingContext ty) (NormalizedDFExpr ty)
-- (Sebastian to himself): Implement these damn types already!!!
-- REMINDER: 
  -- fTy : contains ArgType = TypeVar | Type ty | TupleTy (NonEmpty (ArgType ty)) if function is typed
  -- vars: are data DFVar = 
    --                DFEnvVar :: ArgType ty -> Lit ty -> DFVar 'Data ty
    --                DFVar :: ArgType ty -> ABinding a -> DFVar a ty

typeBottomUp pf@(Let (PureDFFun out@(Direct outBnd) f@(FunRef fun _fid _fTy) inputs@(DFVar _fstTy fstBnd :| scndIn:_)) inCont)
  -- In this case, the function is an Ohua control node. Those nodes allways take two inputs
  -- an nat (0 or 1) and a variable and outputs the variable based on the signal.
  
  -- In a bottom up pass, we've seen the function using the output already.
  -- So we can try to type the input, using the output. 
  -- Also we know the controle signal type
  | fun == Refs.ctrl = do
            -- ctrl:: (bool, nat) -> A -> A
            traceM  $ "Typing controle function " <> show fun
            knownVars <- get

            -- We can type the control input cause it has to be a (bool, nat)
            let ctrlInput' =  DFVar controlSignalType fstBnd
            updateContext fstBnd controlSignalType 
                        -- We can type the data input using the output, cause they have to have the same type
            let dataInput = case scndIn of 
                  (DFVar ty bnd) -> 
                      case HM.lookup (unwrapABnd outBnd) knownVars of
                          Just (Exists (DFVar ty' _)) -> DFVar ty' bnd
                          Just (Exists (DFEnvVar ty' _)) ->  DFVar ty' bnd
                          Nothing -> scndIn
                  (DFEnvVar _ _) -> scndIn
                
            case dataInput of 
               (DFVar ty bnd) -> updateContext bnd ty
               _  -> return ()
  
            return $ Let (PureDFFun out f (ctrlInput' :| [dataInput])) inCont

  | fun == Refs.collect = do
            -- collect :: (nat, A) -> [A]
            aVar <- maybeUpdate outBnd scndIn 

            let natVar = DFVar TypeNat fstBnd
            updateContext fstBnd TypeNat

            return $ Let (PureDFFun out f (natVar :| [aVar])) inCont
  | fun == Refs.runSTCLangSMap = do
            traceM "Typing rustSTCLangSMap function"
            -- This node collects state mutations from a loop i.e. it might be the last point in code 
            -- where this state is used and hence we won't get type information 'bottom up' here. 
            -- But we can try while we're at it 
            let ctrlVar' = DFVar TypeBool fstBnd
            modify (HM.insert (unwrapABnd fstBnd) $ Exists ctrlVar')
            dataInp' <- maybeUpdate outBnd scndIn
            return $ Let (PureDFFun out f (ctrlVar' :| [dataInp'])) inCont

  | fun == Refs.select = do
            -- select :: bool -> A -> A -> A
            traceM $ "Typing select"
    
            knownVars <- get
            let realOutTy = case out of
                    Direct something -> case HM.lookup (unwrapABnd something) knownVars of
                      Just (Exists (DFVar ty' _)) -> ty'
                      Just (Exists (DFEnvVar ty' _)) -> ty'
                      Nothing -> TypeVar
                    otherwise -> undefined

            let realFunType = TypeBool :| [realOutTy, realOutTy]
            -- Type Input vars 
            let typedIns' = NE.map (\case
                      (DFVar _ bnd, ty') ->  DFVar ty' bnd
                      (DFEnvVar _ lit, ty') -> DFEnvVar ty' lit
                  ) $ NE.zip inputs realFunType
            -- Then we add all the variables and their newly assigned types  to the context
            mapM_ (\case
                    (DFVar ty bnd) -> updateContext bnd ty 
                    _literal -> return ()
                ) typedIns'
            return$ Let (PureDFFun out (FunRef fun _fid (FunType (Right realFunType))) typedIns') inCont

  | fun == Refs.seqFun = do
            -- seq:: ([A], Unit) -> Unit 
            -- The Problem is, seq, doesn'T pass on the type information of A so we can't
            -- type it properly by it's input vs. output type as the other Ohua functions
            -- FIXME: I'll pretend it's always list of unit assuming the only purpose of this function
            -- in combination with the collect function is to have certain steps happen list-length times.
            let listInput = DFVar (TypeList TypeUnit) fstBnd
            updateContext fstBnd (TypeList TypeUnit)

            return $ Let (PureDFFun out f (listInput :| [scndIn])) inCont
              
  | otherwise = do
            traceM "Hit a normal function. Should learn from function type"
            -- ToDo: It's not a good idea to pattern match on the number of arguments. 
            -- Because this way we have to tread normal function twice. 
            return pf

typeBottomUp _fo@(Let (PureDFFun out f@(FunRef fun _fId (FunType (Right inputTypes))) vars) inCont) = do
  -- We hit a select function of type (bool, a, a) -> a. So we type it's first input type
  -- and try to derive the second and third from their usages saved in the context
    traceM $ "Typing pure function " <> show fun
    -- We hit any pure function. So we check it's input types and annotate the corresponding variable names.
    let dataInps' = NE.map (\case
                              (DFVar _ bnd, ty') -> DFVar ty' bnd
                              (DFEnvVar _ lit, ty') -> DFEnvVar ty' lit
                          ) $ NE.zip vars inputTypes
    -- Then we add all the variables and their newly assigned types (which may still be TypeVar) to the context
    -- As we go bottom up, those variables will be the output of some function in outer scope, so we can type these
    -- fuctions output then.
    mapM_ (\case
            (DFVar ty bnd) -> updateContext bnd ty
            _literal -> return ()
        ) dataInps'
    return $ Let (PureDFFun out f dataInps') inCont

typeBottomUp e'@(Let (PureDFFun _out (FunRef bnd _ funType) params) _inCont) = do
  -- We hit a pure function that is either Untyped, or has no inputs (FunType Left Unit)
  -- So we can not learn from it's type.
  -- Yet we are in the bottom up pass so it's output might have been typed already
  -- ToDo: Update output type
  traceM $ "Not typing pure function: " <> show bnd
  traceM $ "having type: " <> show funType
  traceM $ "and Params: " <> show params
  return e'

-- SMap
typeBottomUp (Let (SMapFun out@(Just (Direct dOut), _, _) (DFVar ty scndBnd)) inCont) = do
      traceM $ "Typing smapFun"
      -- Smap is used to collect actions on a state from a loop
      -- This means, it is guaranteed, the the state was used (type assigned) before, but
      -- not necessarily after SMap. Hence it makes more sense to type it in a top down, than in a bottom up pass.
      vars <- get
      let dataInp' = case HM.lookup (unwrapABnd dOut) vars of
            Just (Exists (DFVar ty' _)) -> DFVar ty' scndBnd
            _ -> DFVar ty scndBnd
      modify (HM.insert (unwrapABnd scndBnd) $ Exists dataInp')
      return $ Let (SMapFun out dataInp') inCont

typeBottomUp e'@(Let (SMapFun (outData, outControle, outSize) inp) inCont) = do
  traceM $ "Not typing SMap function" <> show e'
  return e'

-- Stateful Functions
typeBottomUp (Let (StateDFFun (mState, mData) f@(FunRef fun _ (STFunType sty tyInfo)) stateIn dataIn) inCont) = do
  traceM $ "Typing stateful function " <> show fun <> " on obj type " <> show sty
  let stateIn' = case stateIn of
        (DFVar ty bnd) -> DFVar (maxType ty sty) bnd
  let dataIn' = case tyInfo of
        (Right info) -> NE.map (\case
                                    (DFVar _ bnd, ty') -> DFVar ty' bnd
                                    (DFEnvVar _ lit, ty') -> DFEnvVar ty' lit
                                ) $ NE.zip dataIn info
        (Left Unit) -> dataIn
  -- Update State type
  case stateIn' of
    (DFVar ty bnd) -> do 
      -- traceM $ "Updating input state" <> show bnd
      updateContext bnd (maxType ty sty)
      case mState of
          Just (Direct stateBnd) -> 
              -- traceM $ "Updating output state" <> show stateBnd
              updateContext stateBnd (maxType ty sty)
          Just (Destruct stateBnds) -> error $ "Saw destruct of " <> show (NE.length stateBnds) <> ". Please report/handle!"
          Just (Dispatch stateBnds) -> error $ "Saw dispatch of "<> show (NE.length stateBnds) <> ". Please report/handle!"
          Nothing -> return () 
  mapM_ (\case
            v@(DFVar ty bnd) -> updateContext bnd ty 
            _ -> return ()
        ) dataIn'
  -- That's it here. We can't learn the/from the potentially existing mData output
  return $ Let (StateDFFun (mState, mData) f stateIn' dataIn') inCont

typeBottomUp e'@(Let (StateDFFun _oBnds _stFun _stateIn _dataIn) _inCont)  = do
  traceM $ "Not typing StateDF function" <> show e'
  return e'
  
-- Recursion
typeBottomUp (Let (RecurFun finalOut recCtrl argOuts initIns recIns cond result) inCont) = do
  traceM $ "Typing RecurFun. Output is" <> show argOuts 
  knownVars <- get
  -- using initins doesn't work (for now, because the literals are also TypeVar)
  -- let (initIns', recIns') = OV.unzip $ OV.map updateVars $ OV.zip initIns recIns
  let _local_updata_fun_because_sure_we_have_to_wrap_everything_in_as_many_dependent_types_as_possibe_TF = 
          \(refBnd, inVar) ->
                      case refBnd of
                        Direct bnd -> 
                          let knownOut = HM.lookup (unwrapABnd bnd) knownVars 
                          in case knownOut of
                            Just (Exists reference) -> 
                              fst $ updateVars (inVar, reference)
                            Nothing -> inVar
                          -- newVar <- maybeUpdate bnd inVar
                          -- newVar
                        Destruct _ -> fuckIt
                        Dispatch _ -> fuckIt
          
  --let newInits = OV.map (\tpl -> do x <- function_I_want_to_use; x) $ (OV.zip argOuts initIns)
  --let newRets = OV.map function_I_want_to_use (OV.zip argOuts recIns)

  let initIns' = OV.map

  let cond' = case cond of
        (DFVar _ty bnd) -> DFVar TypeBool bnd
        (DFEnvVar _ty lit) -> DFEnvVar TypeBool lit

  return $ Let (RecurFun finalOut recCtrl argOuts newInits newRets cond' result) inCont

typeBottomUp _e'@(Let (IfFun (Direct o1, Direct o2) inVar ) inCont) = do
  traceM $ "Tying ifFun"
  -- We know that ifFun is type bool -> (control signal, controle signal).
  -- So we can make sure the input is typed correctly. 
  -- We also know the output type obv. This is not particularly useful in bottom up pass because we've probably typed the two outputs
  -- already, but we can make sure non the less
  
  let inVar' = case inVar of 
          (DFVar _ty bnd) -> trace ("Typing input "<> show bnd<> " of IfFun")  DFVar TypeBool bnd
          other_v -> other_v -- otherwise its a DFEnvVar
  
  case inVar' of 
        (DFVar ty bnd) -> updateContext bnd ty 
        _ -> return()

  -- We know the types of the outputs so just to be sure
  updateContext o1 controlSignalType -- (HM.insert (unwrapABnd o1) $ Exists (DFVar controlSignalType o1))
  updateContext o2 controlSignalType 
  return $ Let (IfFun (Direct o1, Direct o2) inVar' ) inCont

typeBottomUp e'@(Let (IfFun _  _ ) _) = error $ "Generated IfFun produces wrong outputs" <> show e'

typeBottomUp e'@(Var bnd ) = do
      ctxt <- get
      _ <- case HM.lookup returnBinding ctxt of
        Just (Exists (DFVar algoReturnType _)) -> do 
          -- we want the return value to have the return type
          updateContext (DataBinding bnd) algoReturnType
          -- we want to be sure, only the  return value get's the return type this way
          modify (HM.delete returnBinding)
        _ -> error $ "Trying to type " <> show bnd <>" but the rturn type has already been taken. This is probabl a compiler error."
      return e'

typeTopDown:: NormalizedDFExpr ty ->
  State (BindingContext ty) (NormalizedDFExpr ty)
{-typeTopDown (Let (SMapFun out@(Just (Direct dOut), _, _) (DFVar ty scndBnd)) inCont) = do
      -- So we try to type smapFun  again. 
      traceM $ "Typing smapFun again"
      vars <- get 
      traceM $ " Input vars are " <> show scndBnd
      let dataInp' = case HM.lookup (unwrapABnd dOut) vars of
            Just (Exists (DFVar ty' _)) -> trace ("Found Output Type "<> show ty')  DFVar ty' scndBnd
            _ -> DFVar ty scndBnd
      -- We don't insert any more
      -- modify (HM.insert (unwrapABnd scndBnd) $ Exists dataInp')
      return $ Let (SMapFun out dataInp') inCont-}

typeTopDown pf@(Let (PureDFFun out@(Direct _outBnd) f@(FunRef fun _ _) (sizeVar :| [DFVar scndTy scndBnd])) inCont) 
  -- So we try to type SMao
  |fun == Refs.runSTCLangSMap = do
        traceM $ "Typing SMap again"
        knownVars <- get
        let dataInp' = case HM.lookup (unwrapABnd scndBnd) knownVars of
              Just (Exists (DFVar ty' _)) -> trace ("Found Output Type "<> show ty')  DFVar ty' scndBnd
              -- ToDo Actually we should fail here
              _ -> DFVar scndTy scndBnd 
        return $ Let (PureDFFun out f (sizeVar :| [dataInp'])) inCont
  | otherwise = return pf

typeTopDown anyExpr = return anyExpr

function_I_want_to_use ::forall b m ty a. MonadState (HashMap Binding (Exists ty)) m => (OutData b, DFVar a ty) -> m( DFVar a ty)
function_I_want_to_use = 
          \(refBnd, inVar) ->
                      case refBnd of
                        Direct bnd -> do
                           newVar <- maybeUpdate bnd inVar
                           return newVar
                        Destruct _ -> fuckIt
                        Dispatch _ -> fuckIt
    




-- | Updates the type signatures of two zipped DFVars to match them, 
--   giving the EnvVar type preference over the (possibly) inferred type of the other DFVar
updateVars :: (DFVar a ty, DFVar b ty) -> (DFVar a ty, DFVar b ty)
updateVars (DFVar t bnd, DFVar t2 bnd2) =
  let newTy = maxType t t2
  in (DFVar newTy bnd, DFVar newTy bnd2)
updateVars (DFVar t bnd, DFEnvVar t2 lit) =
  -- this case should not occur in recurFun and is only here for the sake of completeness
  let newTy = maxType t2 t
  in (DFVar newTy bnd, DFEnvVar newTy lit)
updateVars (DFEnvVar t lit, DFVar t2 bnd2) =
  let newTy = maxType t t2
  in trace ("Updating vars with literals, maxtype is "<> show newTy)(DFEnvVar newTy lit, DFVar newTy bnd2)
updateVars (v1@DFEnvVar{}, v2@DFEnvVar{}) = (v1, v2)

-- | In case the binding is present in the context and in case the varaible is not a literal (yes, it might be)
--   update the type of the variable in the variable and in the scope.
-- FIXME: Check whether we need to adhere to any correspondence among Binding type 'b' and Variable type 'a'
maybeUpdate ::forall b m ty a. MonadState (HashMap Binding (Exists ty)) m => ABinding b -> DFVar a ty -> m (DFVar a ty)
maybeUpdate reference var = do
  knownVars <- get
  let newVar = case var of 
          (DFVar ty bnd) -> case HM.lookup (unwrapABnd reference) knownVars of
                  Just (Exists (DFVar ty' _bnd)) -> DFVar (maxType ty ty') bnd
                  Just (Exists (DFEnvVar ty' _bnd)) ->  DFVar (maxType ty ty') bnd
                  Nothing -> var
          (DFEnvVar _ty _lit) -> var
    
  case newVar of 
      (DFVar ty bnd) -> do 
        _ <- updateContext bnd ty
        return newVar
      _  -> return newVar
  

updateContext:: MonadState (HashMap Binding (Exists ty)) m => ABinding semTy -> ArgType ty -> m ()
updateContext aBnd newType  = do
  traceM $ "Updating binding " <> show aBnd <> " to type " <> show newType
  modify (HM.insert (unwrapABnd aBnd) $ Exists (DFVar newType aBnd)) 

  

-- | Picks a Argtype from two available ones, choosing any type over TypeVar or the first one.
maxType :: ArgType ty -> ArgType ty -> ArgType ty
-- ToDo: host language types should be prefered over TypeNat and TypeBool as well
maxType TypeVar TypeVar = TypeVar
maxType t TypeVar = t
maxType TypeVar t = t
maxType t _  = t

fuckIt = error $ "Sorry I didn't handle that case"