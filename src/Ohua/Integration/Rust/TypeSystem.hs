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
import Ohua.Integration.Rust.TypePropagation

import Control.Exception (assert, throw)

-- | We go through expressions bottom-up and collect types of variables. Bottom-up implies, that we encounter the usage of a variable, bevor it's assignment. 
--   Therefor we can use function argument types, extracted in the step before to annotate the argument variables at their usage site and via the context also in
--   the expression in wich they are bound. There can be variables e.g. the return variable of a function, that are not used as an argument. We can type them also, 
--   by tracing the current 'innermost' return type. 
-- ToDo: Monadify
finalTyping :: ErrAndLogM m => VarType RustVarType -> FrLang.Expr RustVarType -> m (FrLang.Expr RustVarType)
finalTyping returnTy expr =  evalStateT (typeSystem returnTy expr) HM.empty

{-
Types := T
       | HostType
       | T<T>
       | TupTy [T, T, ..., T]
       | T -> T
-}


{-
Environments:
-------------
Gamma ... associates local variables to a type
Delta ... associates function literals to a type
-}

-- | We pass in the return type of the algorithm and the algorithm and recursively whether each expression has the type it's
--   supposed to have such that expected return type and calculated type match, or (because we have Unknown types) that one just more specific than the other
typeSystem :: (ErrAndLogM m, TypeContextM m) => VarType RustVarType ->  FrLang.Expr RustVarType -> m (FrLang.Expr RustVarType)
typeSystem ty = \case
    l@(LetE (VarP bnd vty) e1 e2) -> do
    {-
      Delta, Gamma |– e1: T1     Delta, Gamma, x:(max X T1) |– e2: T2
    ==================================================================
               Delta, Gamma |– let (x:X) e1 e2 : T2
    -}
        traceM $ "Typing " <> show l
        outer_context <- get
        e1' <- typeSystem vty e1 
        let ty' = maxType ty (exprType e1')
        modify (HM.insert bnd ty')
        e2' <- typeSystem ty e2 
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
     Delta, Gamma |– fun: T1 -> T2 -> ... Tn -> Tf    Delta, Gamma |-t1: T1     Delta, Gamma |- t2:T2 ... Delta, Gamma |- tn:Tn
    ============================================================================================================================
                              Delta, Gamma |– fun t1 t2 ... tn : Tf
    -}
        traceM $ "Typing AppE " <> show fun <> show args
        args' <- mapM (typeSystem typeUnknown) args
        -- here ty should be [argsTypes] -> ty
        fun' <- typeSystem (TypeFunction $ FunType (map (const typeUnknown) args) ty) fun 
        let argsTys = map  exprType  args'
        -- ToDO: this check goes into the Lambda case
        funInputTys <- case funType fun' of
             Just fty -> return $ pureArgTypes fty
             Nothing -> throwError $ "Apply Expression has been formed with something that isn't a function but " <> show fun'
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
                Delta, Gamma, p1:T1, p2:T2, ..., pn:Tn |- e:Te
    =================================================================
        Delta, Gamma |- Lamda p1 p2 .. pn. e : T1 -> T2 -> ... -> Tn -> Te
    -}
        outer_ctxt <- get
        traceM $ "Typing LamE " <> show pats <> show expr
        -- if we new argtypes we need to compare them here, but at least the lengths of type arguments and patterns must fit
        let bndsAndTypes = concatMap patTyBnds pats
        mapM_ (\(b, t) -> modify (HM.insert b t)) bndsAndTypes
        -- ToDo: Here ty must be the return type of the expected type
        expr' <- typeSystem ty expr 
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
        Delta, Gamma |- state : S      Delta, Gamma |- method : S -> Tm
    ========================================================================
                  Delta, Gamma |- Bind state method : Tm
    -}
        -- I have no idea how to get an expected type for the state, because return types say nothing about statefulness so we don't know, in the surrounding expression if
        -- we call a stateful or stateless function 
        -- .. ok .. I know -> first check method, we expect the it to be a Stateful function, then check the state we expect it to be the state type of method :-)
        method' <- typeSystem ty method 
        assertE (isJust $ funType method') $ "The function " <> show method' <> " had type " <> show (exprType method') <> " but should have had a function type."
        -- FIXME: replace ty by methods State type after checking it
        state' <- typeSystem ty state
        
        return $ BindE state' method'

    (IfE cond tTrue tFalse) -> do
    {-
        Delta, Gamma |- cond : Bool    Delta, Gamma |- tTrue : T   Delta, Gamma |- tFalse : T
    ===========================================================================================
                    Delta, Gamma |- If cond tTrue tFalse : T
    -}
        cond' <- typeSystem (asHostNormal rustBool) cond
        return $ IfE cond tTrue tFalse

    (WhileE cond body) -> do
    {-
     Delta, Gamma |- cond : Bool       Delta, Gamma |- body : Unit
    ===============================================================
              Delta, Gamma |- While cond body : Unit

    -}
        return $ WhileE cond body

    (MapE loopFun generator) ->  do
    {-
        Delta, Gamma |- generator : T1<T2>     Delta, Gamma, x:T2 |- loopFun : T3
    ===============================================================================
            Delta, Gamma |- MapE loopFun generator : T1<T3>
    -}
        -- Question: We lower the body of a loop ao a Lambda expression, so loopFun has a function type taking T2 and returning T3 doesn't it?
        -- ToDo: IT#s quite likely, that we cannot type the genrator per se, because of iteration syntax sugar, so better type the loop first.
        generator' <- typeSystem ty generator
        -- ToDo: Expected type it generatorOut -> Unit
        loopFun' <- typeSystem ty loopFun 
        let loopType = exprType loopFun'
        let generatorType = exprType generator'
        case loopType of
            TypeFunction (FunType [inTy] outTy ) -> assertE (inTy == generatorType) $ "In MapE " <> show loopFun' <> " " <> show generator' <> "\nReturn and loopinput don't match"
            other -> throwError $ "For Loop bodies should be converted to Lambda expressions and have function types. This one had type " <> show other

        return $ MapE loopFun' generator'

    (StmtE e1 cont) -> do
    {-
        Delta, Gamma |- e1:T1    Delta, Gamma |- cont : T2
    =======================================================
            Delta, Gamma |- Stmt e1 cont : T2
    -}
        -- The expected return type tells us nothing about e1's expected type so :
        e1' <- typeSystem typeUnknown e1 
        cont' <- typeSystem ty cont 
        return $ StmtE e1' cont'

    (SeqE e1 cont) -> do

    {-
    Actually that expression is only introduced after this point and should be eliminated anyways :-/. So we have it for completeness but do not need to handle it actually

        Delta, Gamma |- cont : T   Delta, Gamma |- e1 : T1
    ========================================================
            Delta, Gamma |- Seq e1 cont : T

    -}
        -- same as for Stmt, we have no expectations for e1
        e1' <- typeSystem typeUnknown e1 
        cont' <- typeSystem ty cont 
        return $ SeqE e1' cont'

    (TupE exprs) -> do

    {-
        Delta, Gamma |- e1:T1  Delta, Gamma |- e2:T2   ...   Delta, Gamma |- en: Tn
    ======================================================================================
          Delta, Gamma |- TupE [e1, e2 ... , en] : TupleTy [T1, T2, .. , Tn]

    -}  
        -- ToDo: we expect ty at this point to be a tuple type and we expect the typed exprs to be equal or > than the types in the tuple type 
        exprs' <- mapM (typeSystem ty) exprs
        return $ TupE exprs'

    v@(VarE bnd vty) -> do
    {-
         x:T in Gamma
    ========================
      Delta, Gamma |– x: T
    -}
        ctxt <- get
        traceM $ "Typing " <> show v <> " in context " <> show ctxt
        -- Normally VarE is not annotated, because it's the usage of a variable
        ty' <- case HM.lookup bnd ctxt of
                            Nothing -> throwError $ "Binding " <> show bnd <> " illegally removed from context. That's a bug."
                            Just cty -> return cty
        let ty_calc  = maxType vty ty'
        -- At this point ty_final might still be a type variable
        -- This case can only be handled in Let, because thats where all possible type sources for bnd are handled. To do so, we need to update the context
        
        -- ToDo: Check that type is <= expected return -> insert maxType ty_calc expected into contex, return ty_max
        -- let ty_max = maxType ty_cal ty
        modify (HM.insert bnd ty_calc)
        
        return $ VarE bnd ty_calc

    (LitE l@(FunRefLit _)) -> do
    {-
       l:T in Delta
    ========================
      Delta, Gamma |- l : T
    -}  
        -- ToDo: type should match expected return type
        return $ LitE l

    (LitE l) -> do
    {-
    ==================
      Gamma |- l : HostType
    -}
        -- ToDo: Type should match expcted return type
        return $ LitE l

    e -> do
        traceM $ "Didn't match pattern " <> show e 
        return e



