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
finalTyping :: ErrAndLogM m => FrLang.Expr RustVarType -> m (FrLang.Expr RustVarType)
finalTyping expr = -- do
    -- expr' <- evalStateT (propagateFunAnnotations expr) HM.empty
    -- evalStateT (propagateVarAnnotations expr') HM.empty
    evalStateT (typeSystem expr) HM.empty

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

typeSystem :: (ErrAndLogM m, TypeContextM m) =>  FrLang.Expr RustVarType -> m (FrLang.Expr RustVarType)
typeSystem = \case
    (LetE (VarP bnd ty) e1 e2) -> do
    {-
      Delta, Gamma |– e1: T1     Delta, Gamma, x:(max X T1) |– e2: T2
    ==================================================================
               Delta, Gamma |– let (x:X) e1 e2 : T2
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
     Delta, Gamma |– fun: T1 -> T2 -> ... Tn -> Tf    Delta, Gamma |-t1: T1     Delta, Gamma |- t2:T2 ... Delta, Gamma |- tn:Tn
    ============================================================================================================================
                              Delta, Gamma |– fun t1 t2 ... tn : Tf
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
                Delta, Gamma, p1:T1, p2:T2, ..., pn:Tn |- e:Te
    =================================================================
        Delta, Gamma |- Lamda p1 p2 .. pn. e : T1 -> T2 -> ... -> Tn -> Te
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
        Delta, Gamma |- state : S      Delta, Gamma |- method : S -> Tm
    ========================================================================
                  Delta, Gamma |- Bind state method : Tm
    -}
        state' <- typeSystem state
        method' <- typeSystem method
        assertE (isJust $ funType method') $ "The function " <> show method' <> " had type " <> show (exprType method') <> " but should have had a function type."

        return $ BindE state' method'

    (IfE cond tTrue tFalse) -> do
    {-
        Delta, Gamma |- cond : Bool    Delta, Gamma |- tTrue : T   Delta, Gamma |- tFalse : T
    ===========================================================================================
                    Delta, Gamma |- If cond tTrue tFalse : T
    -}
        cond' <- typeSystem cond
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
        Delta, Gamma |- e1:T1    Delta, Gamma |- cont : T2
    =======================================================
            Delta, Gamma |- Stmt e1 cont : T2
    -}
        e1' <- typeSystem e1
        cont' <- typeSystem cont
        return $ StmtE e1' cont'

    (SeqE e1 cont) -> do

    {-
    Actually that expression is only introduced after this point and should be eliminated anyways :-/. So we have it for completeness but do not need to handle it actually

        Delta, Gamma |- cont : T   Delta, Gamma |- e1 : T1
    ========================================
            Delta, Gamma |- Seq e1 cont : T

    -}
        e1' <- typeSystem e1
        cont' <- typeSystem cont
        return $ SeqE e1' cont'

    (TupE exprs) -> do

    {-
        Delta, Gamma |- e1:T1  Delta, Gamma |- e2:T2   ...   Delta, Gamma |- en: Tn
    ======================================================================================
          Delta, Gamma |- TupE [e1, e2 ... , en] : TupleTy [T1, T2, .. , Tn]

    -}
        exprs' <- mapM typeSystem exprs
        return $ TupE exprs'

    v@(VarE bnd ty) -> do
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
                            Just ty -> return ty
        let ty_final  = maxType ty ty'
        -- At this point ty_final might still be a type variable
        -- This case can only be handled in Let, because thats where all possible type sources for bnd are handled. To do so, we need to update the context
        modify (HM.insert bnd ty_final)
        return $ VarE bnd ty_final

    (LitE l@(FunRefLit _)) -> do
    {-
       l:T in Delta
    ========================
      Delta, Gamma |- l : T
    -}
        return $ LitE l

    (LitE l) -> do
    {-
    ==================
      Gamma |- l : HostType
    -}
        return $ LitE l

    e -> do
        traceM $ "Didn't match pattern " <> show e
        return e



