module Ohua.Core.DFLang.Passes.TypePropagation where

import Data.HashMap.Lazy as HM hiding (map, foldl')
import Ohua.Core.DFLang.Lang hiding (length)
import qualified Ohua.Core.DFLang.Refs as Refs
import Ohua.Core.Prelude
import qualified Ohua.Types.Vector as OV

import qualified Data.List.NonEmpty as NE

data Exists ty = forall semTy.Exists (DFVar semTy ty) 
type BindingContext ty = (HM.HashMap Binding (Exists ty)) 

showContext :: BindingContext ty -> Text
showContext hm = show (HM.keys hm)
{-
controlSignalType :: ArgType ty
controlSignalType = TupleTy $ TypeBool:| [TypeNat]
-}
returnBinding:: Binding
returnBinding = "finalReturnType"


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
--propagateTypes :: NormalizedDFExpr ty -> NormalizedDFExpr ty
--propagateTypes e = evalState (transformExprM typeBottomUp >=> transformExprTDM typeTopDown $ e) HM.empty



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

typeBottomUp (Let (PureDFFun out@(Direct outBnd) f@(FunRef fun _fid fTy) inputs@(DFVar _fstTy fstBnd :| scndIn: _)) inCont)
  -- In this case, the function is an Ohua control node. Those nodes allways take two inputs
  -- an nat (0 or 1) and a variable and outputs the variable based on the signal.
  
  -- In a bottom up pass, we've seen the function using the output already.
  -- So we can try to type the input, using the output. 
  -- Also we know the controle signal type
  | fun == Refs.ctrl = do
            -- ctrl:: (bool, nat) -> A -> A
            -- traceM  $ "Typing controle function " <> show fun
            knownVars <- get

            -- We can type the control input cause it has to be a (bool, nat)
            let ctrlInput' =  DFVar controlSignalType fstBnd
            updateContext fstBnd controlSignalType 
                        -- We can type the data input using the output, cause they have to have the same type
            let dataInput = case scndIn of 
                  (DFVar _ty bnd) -> 
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
            -- FIXME: Current assumption -> A is always Unit, cause the combination of
            -- of collect and seq is merely to keep calculation going when no actual result is returned from the
            -- loop
            let aVar = case scndIn of 
                  (DFVar _ty bnd) -> DFVar TypeUnit bnd
                  (DFEnvVar _ty lit) -> DFEnvVar TypeUnit lit

            case scndIn of 
                  (DFVar _ty scndBnd) -> updateContext scndBnd TypeUnit
                  _ -> return ()

            let natVar = DFVar TypeNat fstBnd
            updateContext fstBnd TypeNat

            return $ Let (PureDFFun out f (natVar :| [aVar])) inCont
            
  | fun == Refs.runSTCLangSMap = do
            -- traceM "Typing rustSTCLangSMap function"
            -- This node collects state mutations from a loop i.e. it might be the last point in code 
            -- where this state is used and hence we won't get type information 'bottom up' here. 
            -- But we can try while we're at it 
            let ctrlVar' = DFVar TypeNat fstBnd
            modify (HM.insert (unwrapABnd fstBnd) $ Exists ctrlVar')
            dataInp' <- maybeUpdate outBnd scndIn
            return $ Let (PureDFFun out f (ctrlVar' :| [dataInp'])) inCont

  | fun == Refs.select = do
            -- select :: bool -> A -> A -> A
            -- traceM $ "Typing select"
    
            knownVars <- get
            let realOutTy = case out of
                    Direct something -> case HM.lookup (unwrapABnd something) knownVars of
                      Just (Exists (DFVar ty' _)) -> ty'
                      Just (Exists (DFEnvVar ty' _)) -> ty'
                      Nothing -> TypeVar
                    _ -> unhandledCaseError

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
            -- traceM "Hit a normal function. Should learn from function type"
            let dataInps' = 
                  case fTy of
                    (FunType (Right ftypes)) -> 
                        NE.map (\case
                              (DFVar _ bnd, ty') -> DFVar ty' bnd
                              (DFEnvVar _ lit, ty') -> DFEnvVar ty' lit
                          ) $ NE.zip inputs ftypes
                    _ -> inputs
          -- Then we add all the variables and their newly assigned types (which may still be TypeVar) to the context
          -- As we go bottom up, those variables will be the output of some function in outer scope, so we can type these
          -- fuctions output then.
            mapM_ (\case
                    (DFVar ty bnd) -> updateContext bnd ty
                    _literal -> return ()
                ) dataInps'
            return $ Let (PureDFFun out f dataInps') inCont

typeBottomUp _fo@(Let (PureDFFun out f@(FunRef _fun _fId (FunType (Right inputTypes))) vars) inCont) = do
  -- We hit a select function of type (bool, a, a) -> a. So we type it's first input type
  -- and try to derive the second and third from their usages saved in the context
    -- traceM $ "Typing pure function " <> show fun
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

typeBottomUp e'@(Let (PureDFFun _out (FunRef _bnd _ _fty) _params) _inCont) = do
  -- We hit a pure function that is either Untyped, or has no inputs (FunType Left Unit)
  -- So we can not learn from it's type.
  -- Also, by definition it's parameters havn't been ued elsewhere, so we can not type
  -- them by context either
  return e'

-- SMap
typeBottomUp smf@(Let (SMapFun out@(_fst,_scnd,_trd) iterableVar ) inCont) = do
  -- Typing smapFun is not usefull. It's outputs are allready typed and it's 
  -- input, the 'iterable something' we iterate over is fused into the smap node.
  -- SO it's never send and we don't need a typed channel for it
  -- (Sebastian) Sadly this is not always so and hence we need this type.
    let iterableVarTy = TypeList TypeVar
    iterableVar' <- case iterableVar of
                      DFVar _ b -> do
                        let v = DFVar iterableVarTy b
                        modify $ HM.insert (unwrapABnd b) $ Exists v
                        return v
                      DFEnvVar _ l -> do
                        let v = DFEnvVar iterableVarTy l
                        case l of
                          (EnvRefLit b) -> modify $ HM.insert b $ Exists v
                          _ -> return ()
                        return v
    return $ Let (SMapFun out iterableVar') inCont

-- Stateful Functions
typeBottomUp (Let (StateDFFun (mState, mData) f@(FunRef _fun _ (STFunType sty tyInfo)) stateIn dataIn) inCont) = do
  -- traceM $ "Typing stateful function " <> show fun <> " on obj type " <> show sty
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
            (DFVar ty bnd) -> updateContext bnd ty 
            _ -> return ()
        ) dataIn'
  -- That's it here. We can't learn the/from the potentially existing mData output
  return $ Let (StateDFFun (mState, mData) f stateIn' dataIn') inCont

typeBottomUp e'@(Let (StateDFFun _oBnds _stFun _stateIn _dataIn) _inCont)  = do
  -- traceM $ "Not typing StateDF function" 
  return e'
  
-- Recursion
typeBottomUp (Let (RecurFun finalOut recCtrl argOuts initIns recIns cond result) inCont) = do
  -- using initins doesn't work (for now, because the literals are also TypeVar)
  -- let (initIns', recIns') = OV.unzip $ OV.map updateVars $ OV.zip initIns recIns
  newInits <- mapM maybeUpdateByOutData (OV.zip argOuts initIns)
  newRets <- mapM maybeUpdateByOutData (OV.zip argOuts recIns)

  let cond' = case cond of 
                DFVar _ty bnd ->  DFVar TypeBool bnd
                DFEnvVar _ty lit -> DFEnvVar TypeBool lit 

  knownVars <- get
  let result' = case result of 
                DFVar ty bnd -> case HM.lookup (unwrapABnd bnd) knownVars of
                  -- REMINDER: We can not take the 'algo return type' because 
                  -- we are inside the algorithm that calls the recursive algo 
                  -- i.e. return of the outer algo need not be the return of the recursion
                  Just (Exists (DFVar ty' _rB)) ->  (DFVar (maxType ty ty') bnd)
                  Just (Exists (DFEnvVar ty' _lit)) -> (DFVar (maxType ty ty') bnd)
                  Nothing -> result
                DFEnvVar _ty lit -> DFEnvVar TypeBool lit 
  
  case cond of
        (DFVar _ty bnd) -> modify (HM.insert (unwrapABnd bnd) $ Exists cond)
        (DFEnvVar _ty _lit) -> return ()

  case result' of
        (DFVar _ty bnd) -> modify (HM.insert (unwrapABnd bnd) $ Exists result')
        (DFEnvVar _ty _lit) -> return ()

  return $ Let (RecurFun finalOut recCtrl argOuts newInits newRets cond' result') inCont

typeBottomUp _e'@(Let (IfFun (Direct o1, Direct o2) inVar ) inCont) = do
  -- We know that ifFun is type bool -> (control signal, controle signal).
  -- So we can make sure the input is typed correctly. 
  -- We also know the output type obv. This is not particularly useful in bottom up pass because we've probably typed the two outputs
  -- already, but we can make sure non the less
  
  let inVar' = case inVar of 
          (DFVar _ty bnd) -> DFVar TypeBool bnd
          other_v -> other_v -- otherwise its a DFEnvVar
  
  case inVar' of 
        (DFVar ty bnd) -> updateContext bnd ty 
        _ -> return()

  -- We know the types of the outputs so just to be sure
  updateContext o1 controlSignalType -- (HM.insert (unwrapABnd o1) $ Exists (DFVar controlSignalType o1))
  updateContext o2 controlSignalType 
  return $ Let (IfFun (Direct o1, Direct o2) inVar' ) inCont

typeBottomUp (Let (IfFun (_outs) _inVar ) _inCont) = error $ "Encountered ill formed output for if-expression"<>
  ". Maybe you used an if without an else branch or if iside a loop, which currently doesn't work. Otherwise it's a compiler bug. Please report then"


typeBottomUp e'@(Let (SelectFun _out _sign _inOne _inTwo) _inCont) = 
  -- Currently not used 
  return e'
typeBottomUp e'@(Let (CtrlFun _out _sigIn _dataIn) _inCont) = 
  -- Currently not used 
  return e'
typeBottomUp e'@(Let (CollectFun _out _sizeIn _unitIn ) _inCont ) = 
    -- Currently not used 
  return e'

typeBottomUp (Var bnd _ty) = do
      ctxt <- get
      let newReturnType =  case HM.lookup returnBinding ctxt of
              Just (Exists (DFVar returnTy _rB)) -> returnTy
              _ -> error $ "Trying to type " <> show bnd <>" but the return type has already been taken. This is probably a compiler error."
      
      -- we want the return value to have the return type
      updateContext (DataBinding bnd) newReturnType 
      -- we want to be sure, only the  return value get's the return type this way
      modify (HM.delete returnBinding)
      return $ Var bnd newReturnType


maybeUpdateByOutData ::forall b m ty a. MonadState (HashMap Binding (Exists ty)) m => (OutData b, DFVar a ty) -> m( DFVar a ty)
maybeUpdateByOutData = 
          \(refBnd, inVar) ->
                      case refBnd of
                        Direct bnd -> do
                           newVar <- maybeUpdate bnd inVar
                           return newVar
                        Destruct _ -> unhandledCaseError
                        Dispatch _ -> unhandledCaseError
    

-- | Updates the type signatures of two zipped DFVars to match them, 
--   giving the EnvVar type preference over the (possibly) inferred type of the other DFVar
updateVars :: (DFVar a ty, DFVar b ty) -> (DFVar a ty, DFVar b ty)
updateVars (DFVar t bnd, DFVar t2 bnd2) =
  let newTy = maxType t t2
  in (DFVar newTy bnd, DFVar newTy bnd2)
updateVars (DFVar t bnd, DFEnvVar t2 lit) =
  -- this case should not occur in recurFun and is only here for the sake of completeness
  let newTy = maxType t t2
  in (DFVar newTy bnd, DFEnvVar newTy lit)
updateVars (DFEnvVar t lit, DFVar t2 bnd2) =
  let newTy = maxType t t2
  -- in trace ("Updating vars with literals, maxtype is "<> show newTy)(DFEnvVar newTy lit, DFVar newTy bnd2)
  in (DFEnvVar newTy lit, DFVar newTy bnd2)
updateVars (v1@DFEnvVar{}, v2@DFEnvVar{}) = (v1, v2)

-- | In case the binding is present in the context and in case the varaible is not a literal (yes, it might be)
--   update the type of the variable in the variable and in the scope.
-- FIXME: Check whether we need to adhere to any correspondence among Binding type 'b' and Variable type 'a'
maybeUpdate ::forall b m ty a. MonadState (HashMap Binding (Exists ty)) m => ABinding b -> DFVar a ty -> m (DFVar a ty)
maybeUpdate reference var = do
  knownVars <- get
  -- traceM $ "reference: " <> show reference
  -- traceM $ "known vars: "
  -- mapM (traceM . show) $ HM.keys knownVars
  let newVar = case var of 
          (DFVar ty bnd) -> case HM.lookup (unwrapABnd reference) knownVars of
                  Just (Exists (DFVar ty' _bnd)) -> DFVar (maxType ty ty') bnd
                  Just (Exists (DFEnvVar ty' _bnd)) -> DFVar (maxType ty ty') bnd
                  Nothing -> var
          (DFEnvVar _ty _lit) -> var
    
  case newVar of 
      (DFVar ty bnd) -> do 
        _ <- updateContext bnd ty
        return newVar
      _  -> return newVar
  

updateContext:: MonadState (HashMap Binding (Exists ty)) m => ABinding semTy -> ArgType ty -> m ()
updateContext aBnd newType  = do
  -- traceM $ "Updating binding " <> show aBnd <> " to type " <> show newType
  modify (HM.insert (unwrapABnd aBnd) $ Exists (DFVar newType aBnd)) 

  

-- | Picks a Argtype from two available ones, choosing any type over TypeVar or the second one.
-- Problem is, we will assign type twice, when we type recursive functions. In that case we hit the 
-- variable representing the recurFun first, wich is not the actual return value of the algorithm. 
-- Hence we need to override the firsr assignment and it's more intuitive to express this by choosing the first type
-- over the first i.e. maxtype new old. 
-- a
maxType :: ArgType ty -> ArgType ty -> ArgType ty
maxType host_ty@(Type _) _t = host_ty
maxType _t host_ty@(Type _)= host_ty
maxType TypeVar TypeVar = TypeVar
maxType t TypeVar = t
maxType TypeVar t = t
maxType t _  = t


unhandledCaseError :: error
unhandledCaseError = error $ "Sorry I didn't handle that case"
