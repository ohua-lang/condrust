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

-- | We go through expressions bottom-up and collect types of variables. Bottom-up implies, that we encounter the usage of a variable, bevor it's assignment. 
--   Therefor we can use function argument types, extracted in the step before to annotate the argument variables at their usage site and via the context also in
--   the expression in wich they are bound. There can be variables e.g. the return variable of a function, that are not used as an argument. We can type them also, 
--   by tracing the current 'innermost' return type. 
-- ToDo: Monadify
finalTyping :: ErrAndLogM m => FrLang.Expr RustVarType -> m (FrLang.Expr RustVarType)
finalTyping expr = do
    expr' <- evalStateT (propagateFunAnnotations expr) HM.empty
    evalStateT (propagateVarAnnotations expr') HM.empty


typeSystem:: (ErrAndLogM m, TypeContextM m) =>  FrLang.Expr RustVarType -> m (FrLang.Expr RustVarType)
typeSystem = \case
    (LetE (VarP bnd ty) e1 e2) -> do
    {-
        Gamma |– e1: T1  Gamma, x:(max X T1) |– e2: T2  
    =======================================================
        Gamma |– let (x:X) e1 e2 : T2 
    -}
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

        -- assignReturnType e1' ty_final
        -- updateVarType e2' (bnd, ty_final)

        put outer_context
        return $ LetE (VarP bnd ty') e1' e2'


    (AppE f@(LitE (FunRefLit l)) args) -> do
    {-
    Gamma|– f: T1 -> T2 -> ... Tn -> Tf    Gamma|-t1: T1 Gamma|- t2:T2 ... Gamma|- tn:Tn
    ========================================================================================
        Gamma |– f t1 t2 ... tn : Tf
    -}
        args' <- mapM typeSystem args
        -- check that function type and args lengths correspond -> else error 
        -- zipWith maxType function anontation arg types and args' types
        -- update args & f with max types
        -- return Type is handled in let matching
        return $ AppE f args

    (LamE pats expr) -> do
    {-
                Gamma, p1:T1 p2:T2 ... pn:Tn |- e:Te
    =================================================================
        Gamma |- Lamda p1 p2 .. pn. e : T1 -> T2 -> ... -> Tn -> Te
    -}
        return $ LamE pats expr

    (BindE state method) -> do
    {-
            Gamma, state:S p1:T1 p2:T2 ... pn:Tn |- method:Tm
    =================================================================
        Gamma |- Bind state method : S -> T1 -> T2 -> ... -> Tn -> Tm
    -}
        return $ BindE state method

    (IfE cond tTrue tFalse) -> do
    {-
        Gamma |- cond:Bool    Gamma |- tTrue:T   Gamma |- tFalse:T 
    =====================================================================
                    Gamma |- If cond tTrue tFalse : T
    -}
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
        return $ MapE loopFun generator

    (StmtE e1 cont) -> do
    {-
    The return type of e1 is ignored. Nevertheless what happens in e1 is relevant for cont, e1 must be well typed. I'm not sure how to express this         

        Gamma |- cont:T   Gamma |- e1:T1
    ========================================
            Gamma |- Stmt e1 cont : T

    -}
        return $ StmtE e1 cont

    (SeqE e1 cont) -> do

    {-
    Actually that expression is only introduced after this point and should be eliminated anyways :-/. So we have it for completeness but do not need to handle it actually

        Gamma |- cont:T   Gamma |- e1:T1
    ========================================
            Gamma |- Seq e1 cont : T

    -}
        return $ SeqE e1 cont


    (TupE fty exprs) -> do

    {-
        As we now annotate the variables, there's no good reason any more to carry an extra function type for the tuple function but anyways

                      Gamma |- e1:T1  Gamma |- e2:T2 ... Gamma |- en: Tn
    ======================================================================================
      Gamma |- TupE fty [e1, e2 ... , en] : [T1, T2, ... , Tn] -> TupleTy [T1, T2, .. , Tn] 

    -}
        return $ TupE fty exprs


    (VarE bnd ty) -> do

    {-
        x:T in Gamma
    =======================
        Gamma |– x: T
    -}
        ctxt <- get
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



-- ToDo: Refcator to two passes
--       First: bottom up function type propagation 
                -- uppon function applicatcations, try to type arguments with function annotation or vice versa -> fail if that fails
                -- variable usage adds it to context, varibale assignment delets it
propagateFunAnnotations :: (ErrAndLogM m, TypeContextM m) =>  FrLang.Expr RustVarType -> m (FrLang.Expr RustVarType)
propagateFunAnnotations =
    \case
    l@(LetE pat e1 e2) -> do
        traceM $ "\n Working on let: " <> show l
        let realType = realPatType pat
        case realType of
            Just ty -> do
                    -- If pat was annotated, (potential) usages of pat and the corresponding function type arguments are annotated. 
                    -- The return type of e1 might not be annotated, this includes the return type of branches
                    -- > In this case make pat the current return type, proceed with e1 and afterwards with e2
                    ctxt <- get
                    modify (HM.insert currentReturnBnd (FrLang.patType pat))
                    e1' <- propagateFunAnnotations e1
                    -- remove the current return again
                    put ctxt
                    e2' <- propagateFunAnnotations e2
                    return $ LetE pat e1' e2'
            Nothing -> do
                -- If pat was not annotated, we might get it's type from 
                --  a)  the return type of e1 or
                --  b)  usages in e2 (when we concluded argument types from function annotation) written to the context
                -- For any let x = something in cont we want to fail if we don't have a return type for something
                -- so in this case we need to first do e2 and check if pat was used i.e. we can draw the type from the context
                -- set pats type as the current return type if it is known now
                -- then process e1 and fail if we neither have a return type annotated nor a real Type as currentReturn
                -- if we didn't fail, e1 was return type annotated and we can update pats type
                    traceM $ "Pattern is untyped" <> show pat
                    ctxt_outer <- get
                    e2' <- propagateFunAnnotations e2
                    ctxt_e2 <- get
                    traceM $ "Processed continuation. Context is " <> show ctxt_e2
                    let pat' = tryUpdate ctxt_e2 pat
                    put ctxt_outer
                    modify (HM.insert currentReturnBnd (FrLang.patType pat'))
                    e1' <- propagateFunAnnotations e1
                    -- If traversing e1 succeeded the currentReturnType is not Unknown so we might update the binding again 
                    ctxt_e1 <- get
                    let pat'' = tryUpdate ctxt_e1 pat
                    put ctxt_outer
                    -- If pat was of type Unknown AND e1 had no return type the last traversing e1 failed. So as we got here, we might need to update pat's type now.
                    -- We allready typed e2 and the outer expression doesn't know pat, so we do not need to add it to the context
                    modify (HM.insert currentReturnBnd (FrLang.exprType e2'))
                    traceM $ "Concluded pattern is: " <> show pat''
                    return $ LetE pat'' e1' e2'

    AppE fun args -> do
        traceM $ "\n Working on App " <> show fun <> show args
        -- here we have two possible sources of information: 
        --    the arguments might be typed expressions
        --    we might get a function type for fun 
        -- The problem is, if we just traverse them, each one could fail and we don't want to fail here unless they fail both
        -- So what to do? 
        -- First approach: Check if the fun has a type and is fully typed 
                    -- > if so use the argument types as returnType when traversing the arguments, also add funs return type as return Type to the context afterwards
                    -- > if not traverse the arguments setting return type to unknown before each one, this fails if args can't be typedif it succeeds take the expr types of the arguments and the current return type to type the function
            -- Problem, this leaves out the possiblity to type the function by first traversing it  
        outer_context <- get
        case FrLang.funType fun of
            Nothing -> error $ "Couldn't get a function type for the expression:\n " <> show fun <> "\n Please file a bug."
            Just fty -> do
                if fullyTyped fty
                then do
                    traceM $ "Function is fully typed: " <> show fty
                    let argTypes = pureArgTypes fty
                    fun' <- propagateFunAnnotations fun
                    put outer_context
                    args' <- mapM (\(ty, arg) -> do modify (HM.insert currentReturnBnd ty); propagateFunAnnotations arg) (zip argTypes args)
                    modify (HM.insert currentReturnBnd (getReturnType fty))
                    return $ AppE fun' args' -- ToDo: Check if args are typed. 
                                        --       If not, try to type them using the inputType of the 'fun' expression. This can be done by mapping finalTyping over input types and arguments, such that for each 
                                        --       argument the expected input type is the current return type. If this fails -> fail, otherwise currentRetType = exprType fun
                else do
                    traceM $ "Function needs typing: " <> show fty
                    args' <- mapM propagateFunAnnotations args
                    let argTypes = map exprType args'
                    let retTy = case HM.lookup currentReturnBnd outer_context of
                            Nothing -> error $ "No return type for function"
                            Just ty -> trace ("Found return type " <> show ty) ty
                    put outer_context
                    -- ToDo: need to set funtype fo rexpr -> might be fun lit 
                    return $ AppE (setExprFunType fun argTypes retTy) args'


    LamE pats body -> do
        -- The pats are used in the body, so proceed with the body filling the context and update pats typing or fail if that doesn't work, 
        traceM $ "\n Working on lambda" <> show pats
        body' <- propagateFunAnnotations body
        -- we don't want the bound patterns to be typed via the return type because this is no let expression
        modify (HM.insert currentReturnBnd typeUnknown)
        ctxt <- get
        let pats' = map (tryUpdate ctxt) pats
        modify (HM.insert currentReturnBnd (exprType body'))
        return $  LamE pats' body'

    ifE@(IfE cond eTrue eFalse) -> do
        cond' <- propagateFunAnnotations cond
        eTrue' <- propagateFunAnnotations eTrue
        eFalse' <- propagateFunAnnotations eFalse
        if exprType eTrue' == exprType eFalse'
        then do
            modify (HM.insert currentReturnBnd (exprType eTrue'))
            return $  IfE cond' eTrue' eFalse'
        else error $ "Could not derive equal return types for branches in if-expression " <> show  ifE

    WhileE e1 e2 -> return $  WhileE e1 e2 -- Nothing to do, we currently don't use it 
    MapE (LamE [pat] body) generator -> do
        -- MapE maps a function to a generator.We only use it to represent loops at this point.
        -- o we know the first subterm is a LamE and the pattern is untyped (because there are no type annotations on loop patterns)
        -- 1. proceed with the generator 
        -- 2. the return type of the generator is the input type of the function. This way we can type the patterns bound in for loops, even if they are not used in the loop
        -- if we can type the generator -> use the type to type the pattern, otherwise we could also do it the other way arround and try to get the output typ of the generator from a usage of the pattern in the body
        outer_context <- get
        generator' <- propagateFunAnnotations generator
        -- The generators return type is now the currentReturnType in the context
        ctxt <- get
        let pat' = tryUpdate ctxt pat
        case patType pat' of
            t@(Type (HostType Unknown)) -> do
                -- the generator has no known return type, so we process the body first, then try to update the pattern and then try to give the generator a return type
                body' <- propagateFunAnnotations body
                body_ctxt <- get
                -- Now we should be able to update the pattern using the context
                let pat' =  tryUpdate body_ctxt pat
                -- If this didn't error, the pattern now has a type
                let newRet = patType pat'
                -- set the correct return type to the generator
                let generator'' = FrLang.applyToFinal (replaceType newRet) generator'
                put outer_context
                return $ MapE (LamE [pat'] body') generator''
            aType -> do
                -- Insert pattern names and types into context. The pattern might be a tuple, so there can be more than one
                mapM_ (\(bnd, ty) -> modify (HM.insert bnd ty)) (patTyBnds pat')
                body' <- propagateFunAnnotations body
                put outer_context
                return $  MapE (LamE [pat'] body') generator'


    BindE state method -> do
        -- ToDo: 1. This is a usage of the state so check if it is typed 
        --          (problem with states is, that while they are arguments to methods, they apears only with Self type so we can currently only type them upon assigment. 
        --          We might do better in the TypeHandling on impl items by carrying the type for which a method is implemented through)
        --       2. Process the method (it's an AppE expression) 
        --       3. currentRetType = exprType method
        state' <- propagateFunAnnotations state
        method' <- propagateFunAnnotations method
        modify (HM.insert currentReturnBnd (exprType method'))
        return $  BindE state' method'

    StmtE e1 e2  -> do
    -- This just 'does' e1 ignores the return value and procceds with e2. Hence things from e1 might be used in e2 so 
    --       1. proceed with e2
    --       2. proceed with e1
    --       3. currentRetTy = exprype e2
        outer_context <- get
        e2' <- propagateFunAnnotations e2
        e1' <- propagateFunAnnotations e1
        put outer_context
        modify (HM.insert currentReturnBnd (exprType e2'))
        return $ StmtE e1' e2'

    SeqE e1 e2 -> do
    -- Process e2 than process e1
    -- Question: What was the difference to StmtE again?
        outer_context <- get
        e2' <- propagateFunAnnotations e2
        e1' <- propagateFunAnnotations e1
        put outer_context
        modify (HM.insert currentReturnBnd (exprType e2'))
        return $ SeqE e1' e2'

    TupE funTy args -> do
        -- That's the usage of args, not their assignment so we do not need to reset the outer context after processing them 
        args' <- mapM propagateFunAnnotations args
        let argTys@(t:tys) = map exprType args'
        modify (HM.insert currentReturnBnd (TupleTy (t:| tys)))
        return $ TupE (FunType argTys (TupleTy (t:| tys))) args

    l@(LitE lit) -> do
        traceM $ "\n Working on Literal: " <> show l
        case FrLang.exprType l of
            t@(Type (HostType Unknown)) -> do
                    traceM $ "Literal " <> show l <> " had unknown return type."
                    modify (HM.insert currentReturnBnd t)
                    return $ LitE lit --throwError $ "Couldn't type literal " <> show lit <> "Found type"<> show x <>". Please help me out by providing annotations or report error"
            someType -> do
                    modify (HM.insert currentReturnBnd someType)
                    return $ LitE lit

    e@(VarE bnd ty) -> do
        -- Either current return or the variable must be typed if so -> take type, otherwise -> fail 
        traceM $ "\n Working on Var: " <> show e
        ctxt <- get
        if isUnknown ty
        then
            case HM.lookup currentReturnBnd ctxt of
            Nothing -> throwError $ "Couldn't type " <> show bnd
            Just ty' | isUnknown ty' ->  throwError $ "Couldn't type " <> show bnd
            Just ty' -> return $ VarE bnd ty' -- That should be the return variable of the algo because we treat all other variables in the context of their expression (binding/or usage)
        else
            do
            traceM $ "Type is known, will insert binding to context"
            modify (HM.insert bnd ty)
            modify (HM.insert currentReturnBnd ty)
            return e

propagateVarAnnotations :: (ErrAndLogM m, TypeContextM m) =>  FrLang.Expr RustVarType -> m (FrLang.Expr RustVarType)
propagateVarAnnotations = return

tryUpdate::  VarTypeContext -> FrLang.Pat RustVarType -> FrLang.Pat RustVarType
-- We want to update the pattern bound on a LHS of a Let expression
-- The two possible sources of type information are a) usage sites, in which case the name of the binding should be in the context 
-- and b) the return type of the RHS in which case the currentReturn type should be avaiblable and not of type Unknown 
tryUpdate ctxt = \case
    (FrLang.VarP bnd (Type (HostType Unknown))) -> do
        let mReturnType = HM.lookup currentReturnBnd ctxt
            mUsedType = HM.lookup bnd ctxt
        case (mReturnType, mUsedType) of
            (Just rty, _ ) | not (isUnknown rty) -> VarP bnd rty
            (_ , Just uty) | not (isUnknown uty) -> VarP bnd uty
            _  -> error $ "Couldn't type binding " <> show bnd
    tp@(FrLang.TupP pats) | (isNothing (realPatType tp)) -> do
            let mReturnType = HM.lookup currentReturnBnd ctxt -- Current return type could be a tuple type
                mUsedTypes = mapM (>>= (`HM.lookup` ctxt)) (FrLang.patBnd tp)
            case (mReturnType, mUsedTypes) of
                (Just rty, _ ) | not (isUnknown rty) -> error "We haven't implemented destruction of Rust tuple types as function return types. Please remind us to fix this." --return (VarP bnd rty)
                (_, Just (t:ts)) -> setPatType (TupleTy (t:|ts)) tp
                _  -> error $ "Couldn't type tuple binding " <> show tp
    vTyped@FrLang.VarP{} -> vTyped
    wp@FrLang.WildP{} -> wp  -- There's no value in typing a wild pattern becaus ewe type right to left here and wp's aren't used downstream




realPatType:: FrLang.Pat RustVarType -> Maybe (VarType RustVarType)
realPatType = \case
      FrLang.VarP _bnd  ty -> if isUnknown ty then Nothing else Just ty
      FrLang.WildP ty -> if isUnknown ty then Nothing else Just ty
      tp@(FrLang.TupP pats) ->
            let TupleTy tys = FrLang.patType tp
            in if any isUnknown tys then Nothing else Just (TupleTy tys)

replaceType :: VarType RustVarType -> FrLang.Expr RustVarType -> FrLang.Expr RustVarType
replaceType vt e = case e of
  VarE bnd _ty -> VarE bnd vt
  LitE (EnvRefLit b _ty) -> LitE (EnvRefLit b vt)
  LitE (FunRefLit (FunRef q i fty)) -> LitE (FunRefLit (FunRef q i  (setReturnType vt fty)))
  lit@(LitE l) -> if getLitType l == vt
                    then error $ "Return type of function " <> show vt <> " doesn't match type "<> show (getLitType l) <> " of returned literal"
                    else lit
                  -- ToDo: We can also get TupE here, which needs to be handled

currentReturnBnd::Binding
currentReturnBnd = "InternalCurrentReturn"

-- Question: Can we have functions as arguments? 
fullyTyped :: FunType RustVarType -> Bool
fullyTyped (FunType args retTy) = all (\case  (Type (HostType Unknown)) -> False; TypeFunction fty -> fullyTyped fty; _ -> True) (retTy: args)
fullyTyped (STFunType sTy argTys retTy) = all (\case (Type (HostType Unknown)) -> False; TypeFunction fty -> fullyTyped fty; _ -> True) (sTy: retTy: argTys)