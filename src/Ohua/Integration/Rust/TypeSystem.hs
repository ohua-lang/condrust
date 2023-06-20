{-# LANGUAGE LambdaCase #-}

module Ohua.Integration.Rust.TypeSystem where


import qualified Data.HashMap.Lazy as HM

import Ohua.Prelude hiding (getVarType)

import Ohua.Frontend.Types
import Ohua.Frontend.PPrint ()
import Ohua.Frontend.Lang as FrLang
    ( Expr(..),
      Pat(TupP, VarP, WildP),
      exprType,
      patType,
      patBnd,
      patTyBnds,
      funType,
      setPatType,
      setExprFunType,
      applyToFinal)

import Ohua.Integration.Rust.TypeHandling
import Foreign.C (throwErrno)
import Control.Exception (assert, throw)

-- | We go through expressions bottom-up and collect types of variables. Bottom-up implies, that we encounter the usage of a variable, bevor it's assignment. 
--   Therefor we can use function argument types, extracted in the step before to annotate the argument variables at their usage site and via the context also in
--   the expression in wich they are bound. There can be variables e.g. the return variable of a function, that are not used as an argument. We can type them also, 
--   by tracing the current 'innermost' return type. 
-- ToDo: Monadify
finalTyping :: ErrAndLogM m => FrLang.Expr RustVarType -> m (FrLang.Expr RustVarType)
finalTyping expr = -- do
    -- expr' <- evalStateT (propagateFunAnnotations expr) HM.empty
    -- evalStateT (propagateVarAnnotations expr') HM.empty
    evalStateT (typeSystem expr) HM.empty


typeSystem:: (ErrAndLogM m, TypeContextM m) =>  FrLang.Expr RustVarType -> m (FrLang.Expr RustVarType)
typeSystem = \case
    (LetE (VarP bnd ty) e1 e2) -> do
    {-
        Gamma |– e1: T1  Gamma, x:(max X T1) |– e2: T2  
    =======================================================
        Gamma |– let (x:X) e1 e2 : T2 
    -}
        traceM $ "Typing Let " <> show bnd <> " = " <> show e1 <> " in\n    " <> show e2
        outer_context <- get
        e1' <- typeSystem e1
        let ty' = maxType ty (exprType e1')
        modify (HM.insert bnd ty')
        e2' <- typeSystem e2
        -- If we didn't fail up to here, the expression is well-typed 

        inner_context <- get
        -- We assume that processing e2 updated the binding in the context if it was used with a concrete type
        ty_final <- case HM.lookup bnd inner_context of
                            Nothing -> throwError $ "Binding " <> show bnd <> " illegally removed from context. That's a bug."
                            Just ty | isUnknown ty -> throwError $ "Type for binding " <> show bnd <> " could not be inferred. Please provide annotation."
                            Just ty -> return ty
        traceM $ "Final type of bnd "<> show bnd <> " is type  " <> show ty_final

        e1'' <- propagateType ty_final e1'

        -- That happened during processing e2
        -- updateVarType e2' (bnd, ty_final)

        put outer_context
        traceM $ "Returing Let " <> show (VarP bnd ty_final) <> " = " <> show e1'' <> " in\n    " <> show e2'
        return $ LetE (VarP bnd ty_final) e1'' e2'



    (AppE fun args) -> do
    {-
    Gamma|– fun: T1 -> T2 -> ... Tn -> Tf    Gamma|-t1: T1 Gamma|- t2:T2 ... Gamma|- tn:Tn
    ========================================================================================
        Gamma |– fun t1 t2 ... tn : Tf
    -}
        traceM $ "Typing AppE " <> show fun <> show args
        args' <- mapM typeSystem args
        fun' <- typeSystem fun
        let argsTys = map exprType args'
        funInputTys <- case funType fun' of
             Just fty -> return $ pureArgTypes fty
             Nothing -> throwError $ "Apply Expression haas been formaed with something that isn't a function but " <> show fun'
        -- check that function type and args lengths correspond -> else error
        assertE (length argsTys == length funInputTys) $ "Number of given arguments doesn't fit to number of arguments from declaration in fucntion call: " <> show fun
                                               <> "\nWith arguments: " <> show args
        let maxArgTypes =  zipWith maxType argsTys funInputTys
        -- maxArgTypes will fail if there was a typing inconsistency between args and function annotation
        -- update args & f with max types
        -- I was tempted to delete args from the context here, but this is not entirely top-down i.e. we can not use this to make sure the 'used arguments' aren't valid anymore downstream
        -- but I need to update the context for any args, that where named variables or literals
        args'' <- zipWithM propagateReturnType maxArgTypes args'
        -- the function could be a literal, in which case type propagation is trivial. But it could also be a Lambda expression, in which case we handled the body
        -- before, possibly without propper types in the context, in case the types came from the arguments. :-( 
        let fun'' = propagateArgTypes maxArgTypes fun'
        -- the functions return type is handled in the LetE matching
        traceM $ "Returning AppE " <> show fun'' <> show args''
        return $ AppE fun'' args''

    (LamE pats expr) -> do
    {-
                Gamma, p1:T1 p2:T2 ... pn:Tn |- e:Te
    =================================================================
        Gamma |- Lamda p1 p2 .. pn. e : T1 -> T2 -> ... -> Tn -> Te
    -}
        outer_ctxt <- get
        traceM $ "Typing LamE " <> show pats <> show expr
        let bndsAndTypes = concatMap patTyBnds pats
        mapM_ (\(b, t) -> modify (HM.insert b t)) bndsAndTypes
        expr' <- typeSystem expr
        body_ctxt <- get
        let pats' = map (tryUpdate body_ctxt) pats
        -- Remove the local vars from the context 
        put outer_ctxt
        -- FIXME: By resetting the context, we also delete updated typing for vriables from outer context.
        -- Resetting works if it's just about the names being defined but I think we need another meachnism (maybe intersection based on keys) here
        traceM $ "Returning LamE " <> show pats' <> show expr'
        return $ LamE pats' expr'

    (BindE state method) -> do
    {-
        Gamma |- state:S  Gamma |- method: T1 -> T2 -> ... -> Tn -> Tm
    ========================================================================
         Gamma |- Bind state method : S -> T1 -> T2 -> ... -> Tn -> Tm
    -}  
        state' <- typeSystem state
        method' <- typeSystem method
        assertE (isJust $ funType method') $ "The function " <> show method' <> " had type " <> show (exprType method') <> " but should have had a function type."

        return $ BindE state' method'

    (IfE cond tTrue tFalse) -> do
    {-
        Gamma |- cond:Bool    Gamma |- tTrue:T   Gamma |- tFalse:T 
    =====================================================================
                    Gamma |- If cond tTrue tFalse : T
    -}
        cond' <- typeSystem cond
        return $ IfE cond tTrue tFalse

    (WhileE cond body) -> do
    {-
    While loops (at least in Rust) allways evaluate to ()/Unit

            Gamma |- cond:Bool    
    ======================================= 
        Gamma |- While cond body : Unit

    -}  
        return $ WhileE cond body

    (MapE loopFun generator) ->  do
    {-
    Like while, for-loops (which we represent as MapE) are expressions but can never evaluate to anything but Unit
    

        Gamma |- generator:Tg     Gamma |- loopFun: Tg -> Unit
    ==============================================================
            Gamma |- MapE loopFun generator : Unit

    -}
        generator' <- typeSystem generator
        loopFun' <- typeSystem loopFun
        let loopType = exprType loopFun'
        let generatorType = exprType generator'
        case loopType of
            TypeFunction (FunType [inTy] outTy ) -> assertE (inTy == generatorType) $ "In MapE " <> show loopFun' <> " " <> show generator' <> "\nReturn and loopinput don't match"
            other -> throwError $ "For Loop bodies should be converted to Lambda expressions and have function types. This one had type " <> show other  
        
        return $ MapE loopFun' generator'

    (StmtE e1 cont) -> do
    {-
    The return type of e1 is ignored. Nevertheless what happens in e1 is relevant for cont, e1 must be well typed. I'm not sure how to express this         

        Gamma |- cont:T   Gamma |- e1:T1
    ========================================
            Gamma |- Stmt e1 cont : T

    -}  
        e1' <- typeSystem e1
        cont' <- typeSystem cont
        return $ StmtE e1' cont'

    (SeqE e1 cont) -> do

    {-
    Actually that expression is only introduced after this point and should be eliminated anyways :-/. So we have it for completeness but do not need to handle it actually

        Gamma |- cont:T   Gamma |- e1:T1
    ========================================
            Gamma |- Seq e1 cont : T

    -}
        e1' <- typeSystem e1
        cont' <- typeSystem cont 
        return $ SeqE e1' cont'


    (TupE exprs) -> do

    {-
        As we now annotate the variables, there's no good reason any more to carry an extra function type for the tuple function but anyways

                      Gamma |- e1:T1  Gamma |- e2:T2 ... Gamma |- en: Tn
    ======================================================================================
      Gamma |- TupE [e1, e2 ... , en] : [T1, T2, ... , Tn] -> TupleTy [T1, T2, .. , Tn] 

    -}  
        exprs' <- mapM typeSystem exprs
        return $ TupE exprs'


    v@(VarE bnd ty) -> do
    {-
        x:T in Gamma
    =======================
        Gamma |– x: T
    -}
        ctxt <- get
        traceM $ "Typing " <> show v <> " in context " <> show ctxt
        -- Normally VarE is not annotated, because it's the usage of a variable
        ty' <- case HM.lookup bnd ctxt of
                            Nothing -> throwError $ "Binding " <> show bnd <> " illegally removed from context. That's a bug."
                            Just ty -> return ty
        let ty_final  = maxType ty ty'
        -- At this point ty_final might still be a type variable
        -- This case can only be handled in Let, because thats where all possible type sources for bnd are handled. To do so, we need to update the context
        modify (HM.insert bnd ty_final)
        return $ VarE bnd ty_final

    (LitE l) -> do
    {-
    Usually literals just have types, e.g. True is Bool, independent of the context. 
    But we also have function names as literals. Which are actually not literals but named terms defined elsewhere, so
    How to distigush inference rules for normal literals, that don't have preconditions in the context fom function literals whose infered type depends on the context

    =============
        l : T
    -}
        return $ LitE l
    e -> do
        traceM $ "Didn't match pattern " <> show e
        return e


propagateType:: (ErrAndLogM m, TypeContextM m) => VarType RustVarType -> FrLang.Expr RustVarType -> m (FrLang.Expr RustVarType)
propagateType ty = \case 
        -- We should have a funtion type at this point
        --- let a: x->y = LamE pats body => pats:x body:y 
        e@LamE{} -> case ty of 
              TypeFunction fty -> propagateFunType fty e
              other -> throwError $ "Cannot type the lambda expression " <> show e <> " with non-function type " <> show ty
        -- ToDo: this is not correct, there might be nested function types.  
        --- let a: x = e => e:x 
        e -> FrLang.applyToFinal (propagateReturnType ty) e

propagateFunType::(ErrAndLogM m, TypeContextM m) => FunType RustVarType -> FrLang.Expr RustVarType -> m (FrLang.Expr RustVarType)
propagateFunType fty (LamE pats body) = do
    let argTypes = pureArgTypes fty 
    let returnType = getReturnType fty
    body' <- propagateType returnType body
    -- FIXME: This isn't correct still. We'd need to add the typed patterns to the context and use them when passing the body again.
    return $ LamE (zipWith setPatType argTypes pats) body'
propagateFunType _fty other           = throwError $ "Cannot apply a function type to expression " <> show other 

propagateReturnType :: (ErrAndLogM m, TypeContextM m) => VarType RustVarType -> FrLang.Expr RustVarType -> m (FrLang.Expr RustVarType)
propagateReturnType tyNew expr = do
    ctxt <- get
    case expr of
        (VarE bnd _t) -> do modify (HM.insert bnd tyNew); return (VarE bnd tyNew)
        LitE (EnvRefLit bnd _ty) -> do modify (HM.insert bnd tyNew); return $ LitE (EnvRefLit bnd tyNew)
        LitE (FunRefLit (FunRef q i fty)) -> return $ LitE (FunRefLit (FunRef q i  (setReturnType tyNew fty)))
        -- ToDo: If we had a HostLiteral (instead of NumericLit etc.) we'd need to type here as well 
        lit@(LitE l) -> return lit
        -- ToDo: implement type return type propagation for missing expressions.
        otherE -> throwError $ "Implementing type propagation for " <> show otherE <> " is required"

propagateArgTypes:: [VarType RustVarType] -> FrLang.Expr RustVarType -> FrLang.Expr RustVarType
propagateArgTypes tys fun  = case fun of
    VarE bnd (TypeFunction fty) ->       let newFty = setFunType tys (getReturnType fty) fty
                                         in  VarE bnd (TypeFunction newFty)
    LitE (FunRefLit (FunRef q i fty)) -> let newFty = setFunType tys (getReturnType fty) fty
                                         in  LitE (FunRefLit (FunRef q i newFty))
    -- ToDo: This doesn't propagate the pattern types into the body, but we likely allready processed the body so just adding the types to the context wont help here 
    -- > FIX by propagation types to body. 
    LamE pats body -> LamE (zipWith setPatType tys pats) body
    BindE state fun -> BindE state (propagateArgTypes tys fun)


tryUpdate::  VarTypeContext -> FrLang.Pat RustVarType -> FrLang.Pat RustVarType
-- We want to update the pattern bound on a LHS of a Let expression
-- The two possible sources of type information are a) usage sites, in which case the name of the binding should be in the context 
-- and b) the return type of the RHS in which case the currentReturn type should be avaiblable and not of type Unknown 
tryUpdate ctxt = \case
    (FrLang.VarP bnd (Type (HostType Unknown))) -> do
        case HM.lookup bnd ctxt of
            Just rty | not (isUnknown rty) -> VarP bnd rty
            _  -> error $ "Couldn't type pattern " <> show bnd
    tp@(FrLang.TupP pats) | (isNothing (realPatType tp)) -> do
            let mUsedTypes = mapM (>>= (`HM.lookup` ctxt)) (FrLang.patBnd tp)
            case  mUsedTypes of
                (Just [rty]) | not (isUnknown rty) -> error "We haven't implemented destruction of Rust tuple types as function return types. Please remind us to fix this." --return (VarP bnd rty)
                (Just tys@(t:ts)) | length tys == length pats -> setPatType (TupleTy (t:|ts)) tp
                _  -> error $ "Couldn't type tuple binding " <> show tp
    vTyped@FrLang.VarP{} -> vTyped
    wp@FrLang.WildP{} -> wp  -- There's no value in typing a wild pattern because we type right to left here and wp's aren't used downstream




realPatType:: FrLang.Pat RustVarType -> Maybe (VarType RustVarType)
realPatType = \case
      FrLang.VarP _bnd  ty -> if isUnknown ty then Nothing else Just ty
      FrLang.WildP ty -> if isUnknown ty then Nothing else Just ty
      tp@(FrLang.TupP pats) ->
            let TupleTy tys = FrLang.patType tp
            in if any isUnknown tys then Nothing else Just (TupleTy tys)


-- ToDo: This is essentially the same as propagateReturnType, except that we have a slightly different context and do not need to 
-- manipulate the context -> Can we merge the functions?
replaceType ::(ErrAndLogM m) => VarType RustVarType -> FrLang.Expr RustVarType -> m (FrLang.Expr RustVarType)
replaceType tyNew e = case e of
  VarE bnd _ty -> return $ VarE bnd tyNew
  LitE (EnvRefLit b _ty) -> return $  LitE (EnvRefLit b tyNew)
  LitE (FunRefLit (FunRef q i fty)) -> return $ LitE (FunRefLit (FunRef q i  (setReturnType tyNew fty)))
  lit@(LitE l) -> if getLitType l == tyNew
                    then throwError $ "Return type of function " <> show tyNew <> " doesn't match type "<> show (getLitType l) <> " of returned literal"
                    else return lit
                  -- ToDo: We can also get TupE here, which needs to be handled


-- Question: Can we have functions as arguments? 
fullyTyped :: FunType RustVarType -> Bool
fullyTyped (FunType args retTy) = all (\case  (Type (HostType Unknown)) -> False; TypeFunction fty -> fullyTyped fty; _ -> True) (retTy: args)
fullyTyped (STFunType sTy argTys retTy) = all (\case (Type (HostType Unknown)) -> False; TypeFunction fty -> fullyTyped fty; _ -> True) (sTy: retTy: argTys)


-- Stole this (kind of) from Core/Util because I don't wantto import Core here
assertE :: (IsString s, MonadError s m, HasCallStack, Monoid s) => Bool -> s -> m ()
assertE True  _ = return ()
assertE False msg = throwError $ "TypingError: " <> msg
{-# INLINE assertE #-}