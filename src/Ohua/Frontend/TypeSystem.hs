{-# LANGUAGE ScopedTypeVariables #-}

module Ohua.Frontend.TypeSystem
  (Delta, toWellTyped)
where


import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE 
import Data.List (nub, last, init, unzip4, map)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

import Ohua.Prelude hiding (Nat, last, init, map)
import qualified Ohua.Prelude as Res
  ( Lit(..), FunType(..), FunRef(..))

import Ohua.Frontend.PPrint (prettyExpr)
import Ohua.Frontend.Lang
    ( UnresolvedExpr
    , ResolvedExpr
    , Expr(..)
    , MethodRepr(..)
    , UnresolvedPat(..)
    , ResolvedPat(..)
    , UnresolvedType
    , ResolvedType
    , Pat(TupP, VarP)
    , patTyBnds
    , patType
    , freeVars
    )
  
import Ohua.Frontend.SymbolResolution (SymResError(..), Delta, Gamma, resolveSymbols)

import Ohua.Types.Vector (Nat(..))


-- TODO config param
exprTraceDepth :: Nat
exprTraceDepth = Succ $ Succ $ Succ Zero

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


toWellTyped :: forall ty m. ErrAndLogM m => Delta ty 'Resolved -> [Import] -> UnresolvedExpr ty -> m (ResolvedExpr ty)
toWellTyped delta modImports e@(LamE pats expr) =
  let
    gamma = HM.fromList $ freeVars e
    -- FIXME: Remove this when algos are Functions of typed inputs, output and body.
    patBndsAndTypes =  (concatMap (NE.toList . patTyBnds) pats) 
    replaceTy = (\(bnd, ty) -> case unresToRes ty of 
                        Just tyR -> Just (bnd, tyR)
                        Nothing -> Nothing)
    resolvedPats = case mapM replaceTy patBndsAndTypes of 
      Just (rp:rps) -> map (\(b, t) -> VarP b t) (rp:rps)
      -- FIXME: Again we have the problem (this time with algos themselves) that unit functions need to be treated separately
      Just [] -> [VarP "_" (IType TypeUnit)]
      _ -> error $ "Could not resolve the types of function arguments in " <> show e
    gamma_with_inpts = foldl (\ g (b, ty) -> HM.insert b ty g) gamma patBndsAndTypes
  in do
    traceM "Gamma (pretty):"
    traceM $ renderStrict $ layoutSmart defaultLayoutOptions $ pretty $ HM.toList gamma_with_inpts
    traceM ""
    traceM "Algo :"
    traceM $ renderStrict $ layoutSmart defaultLayoutOptions $ pretty e
    traceM ""
    (_gamma', e', _ty, imports') <- flip runReaderT (e :| []) $ typeSystem  delta modImports gamma_with_inpts expr
    return (LamE resolvedPats e')
toWellTyped _ _ e = throwError $ "Algorithm was not a lambda expression. Please file a bug. " <> show e

type TypeErrorM m ty = MonadReader (NonEmpty (UnresolvedExpr ty)) m

throwErrorWithTrace :: forall m ty a.
                       (ErrAndLogM m, TypeErrorM m ty)
                    => Text
                    -> m a
throwErrorWithTrace m = do
  exprs <- ask
  throwError $ m <> exprTrace (NE.reverse exprs) exprTraceDepth
  where
    exprTrace :: NonEmpty (UnresolvedExpr ty) -> Nat -> Text
    exprTrace (e:|_ ) Zero = "\nIn expr:\n" <> prettyExpr e
    exprTrace (e:|[]) _    = "\nIn expr:\n" <> prettyExpr e
    exprTrace (e:|(es:ess)) (Succ n) = "\nIn expr:\n" <> prettyExpr e <> exprTrace (es:|ess) n

invariantBroken :: (ErrAndLogM m, TypeErrorM m ty) => Text -> m a
invariantBroken m = throwErrorWithTrace $ "Invariant broken: " <> m

wfError :: (ErrAndLogM m, TypeErrorM m ty) => Text -> m a
wfError m = throwErrorWithTrace $ "Wellformedness error: " <> m

typeError :: (ErrAndLogM m, TypeErrorM m ty) => Text -> m a
typeError m = throwErrorWithTrace $ "Type error: " <> m

symResError :: (ErrAndLogM m, TypeErrorM m ty) => Text -> m a
symResError m = throwErrorWithTrace $ "Symbol resolution error: " <> m

typeExpr :: (ErrAndLogM m, TypeErrorM m ty)
            => Delta ty Resolved
            -> [Import]
            -> Gamma ty Unresolved
            -> UnresolvedExpr ty
            -> m (Gamma ty Resolved , ResolvedExpr ty, OhuaType ty Resolved, [Import])
-- typeExpr delta imports gamma e = local ( <> (e:|[])) $ typeSystem delta imports gamma e
-- We need to solve the following problem here:
-- We get a Lambda expression representing an algorithm. Because it's a Lamda, it doesn't have a function reference attached
-- so we cannot get the parameter types from Delta.
-- Also Type Checking of Lambda expressions will not add parameter types to the context Gamma.
-- That means for any parameter x of an algo, we won't be able to resolve it's type unless we make an exception about
-- adding Lambda parameters to Gamma during type checking ... That's why we do it here.
typeExpr delta imports gamma e = local ( <> (e:|[])) $ typeSystem delta imports gamma e



typeSystem :: forall ty m
           .  (ErrAndLogM m, TypeErrorM m ty)
           => Delta ty Resolved
           -> [Import]
           -> Gamma ty Unresolved
           -> UnresolvedExpr ty
           -> m (Gamma ty Resolved, ResolvedExpr ty, OhuaType ty Resolved, [Import])
typeSystem delta imports gamma = \case
  {-
    Delta, Gamma |– e1: T1     Delta, Gamma, x:(max X T1) |– e2: T2
  ==================================================================
             Delta, Gamma |– let (x:X) e1 e2 : T2
  -}
  (LetE pat e1 e2) -> do
    
    (gamma', e1', tyT1', imports') <- typeExpr delta imports gamma e1
    
    pat' <- typePat pat tyT1'
    
    -- ToDo: We currently can't allways get unresolved types from resolved ones although I think this should be possible.
    -- FIXME: I Think we shouldn't do this conversion but add the (original) unresolved pattern type to e1's and e2' gamma
    --        context and check the rule after typechecking both

    let mResTypes = mapM 
          (\(bnd, tyR) ->  case resToUnres tyR of 
              Just tyU -> Just (bnd, tyU)
              Nothing -> Nothing)
          $ patTyBnds pat'
    
    -- let patTys = mapM (second resToUnres) $ patTyBnds pat'
    let gamma_u' =
          case mResTypes of 
            Just tys -> foldl (\ g (b, t) -> HM.insert b t g) gamma tys 
            Nothing -> 
              error $ "Implementation Error. It should probably be possible to convert " <> show mResTypes <> " to unresolved types but we didn't implement it"

    (gamma'', e2', tyT2, imports'' ) <- typeExpr delta imports' gamma_u' e2
    -- We type bottom up, i.e. using the usage sites of variables to add them to the typed context
    -- this means, we need to unify usages in e1 and e2 to typecheck the outer expressions 
    let gamma''' = HM.union gamma' gamma''  

    return (gamma''' , LetE pat' e1' e2', tyT2, imports'' )

  {-
               Delta, Gamma |– fun: T1 -> T2 -> ... Tm -> Tf  n<=m
     Delta, Gamma |-t1: T1  Delta, Gamma |- t2:T2  ...  Delta, Gamma |- tn:Tn
  ===================================================================================
                  Delta, Gamma |– fun t1 t2 ... tn : Tf
  -}
  (AppE fun args) -> do
      -- traceM $ "Typing application of " <> show fun <> " with args " <> show args
        
      -- First we type the function
      (gamma', fun', funTy, imports' ) <- typeExpr delta imports gamma fun

      -- Then we type it's args
      (gammas , args', argTypesR) <- 
        (unzip4 <$> mapM (typeExpr delta imports gamma) args ) 
          >>= (\ (gamma' , argsT, argTypes , _ ) -> return (gamma', argsT, argTypes))

      -- Produce a common context with all arg types
      let gamma'' = foldl (\ gs g -> HM.union gs g) gamma' gammas
      
      -- Then we check if function decalaration and arg types match
      resTy <- case funTy of
        FType (FunType ins out) -> do
            (_, pendingArgsTy) <- assocArgWithType (toList ins) $ toList argTypesR
            case pendingArgsTy of
              [] -> return out
              -- FIXME: We need a syntax to represent Unit Function that's clearly separate from Functions with one actual argument
              (IType TypeUnit: []) -> return out
              (x:xs)  -> return $ FType  (FunType (x:|xs) out)
        FType (STFunType sin ins out) -> do
            (_, pendingArgsTy) <- assocArgWithType ins $ toList argTypesR
            return $ FType  (STFunType sin pendingArgsTy out)
        t -> typeError $ "First argument of function application is not a function, but has type: " <> show t
      
      -- traceM $ "Found return type to be " <> show resTy
      -- traceM $  renderStrict $ layoutSmart defaultLayoutOptions $ pretty $ HM.toList gamma''
      return (gamma'', AppE fun' args', resTy, imports')
      where  
        -- We must not have more arguments than argument types in the declaration
        assocArgWithType [] (_:_) =  wfError "Too many arguments in function application."
        -- We can have less arguments than types because function application can be partial (e.g. through previous transformation) 
        assocArgWithType l [] = return ([], l)
        -- Argument type and type of argument have to match
        -- FIXME: Actually the given argument type has to be a subtype/specialization of the declared argument type actually. ie. we have to 
        --        add (>=) to the constraints of HostType
        -- for now I'll replace this with a 'compromise compare' function to make sure that we at least not accept the added UnitArg as equal to a 
        -- to the type of an argument wrongly passed to a unit function
        assocArgWithType (t:ts) (argT:argTs')
           {- (argsAndTy, pendingTy) <- assocArgWithType ts argTs'
            return ((argT,t) : argsAndTy, pendingTy)-}
          | compromise_compare t argT = do
            (argsAndTy, pendingTy) <- assocArgWithType ts argTs'
            return ((argT,t) : argsAndTy, pendingTy)
          | otherwise = typeError $ "Function argument type "<> show t <> " and type of given argument " <> show argT <> " do not match."


  {-
              Delta, Gamma, p1:T1, p2:T2, ..., pn:Tn |- expr:Te
  ============================================================================
      Delta, Gamma |- Lambda p1 p2 .. pn. expr : T1 -> T2 -> ... -> Tn -> Te
  -}
  e@(LamE pats expr) ->
    let
      -- FIXME: Replace the check for "_" binding here. We currently introduce it when 
      -- as unit arg representation, but we should have something not stringly typed for that purpose 
      invariantGetGamma :: Binding -> StateT (Gamma ty Resolved) m (OhuaType ty Resolved)
      invariantGetGamma bnd | bnd == fromString "_" = return (IType TypeUnit)
      invariantGetGamma bnd = do
        gam <- get
        case HM.lookup bnd gam of
          -- FIXME:
          -- We cannot error if a pattern isn't present in gamma, the reason is, that gamma will only
          -- contain the types of used variables
          -- This is because we cannot "forward" gamma through the applications of typeExpr i.e. 
          -- when we give it a gamma containing the patterns types, those types are Unresolved and are not just copied to the
          -- resolved gamma we return. the resolved gamma will only contain used (and therefor typed) variables
          Nothing -> invariantBroken $ "Pattern deleted while typing. Deleted pattern: " <> show bnd
          Just ty' -> (modify $ HM.delete bnd) >> return ty'

      typePatFromGamma :: UnresolvedPat ty -> StateT (Gamma ty Resolved) m (ResolvedPat ty)
      typePatFromGamma (VarP bnd t) = (\ t' -> VarP bnd <$> maxType t t') =<< invariantGetGamma bnd
      typePatFromGamma (TupP ps) = TupP <$> mapM typePatFromGamma ps
    in do
      
      let gamma' = foldl (\ g (b, t) -> HM.insert b t g) gamma $ join $ map (NE.toList . patTyBnds) pats
      traceM $ "Gamma inside Lambda :" <> show gamma'
      (gammaR', expr', tyE, imports' ) <- typeExpr delta imports gamma' expr
      -- traceM $ "Gamma after typing the body :" <> show gammaR'
      (pats', gammaR'') <- runStateT (mapM typePatFromGamma pats) gammaR'
      
      let funty = case pats' of 
           []       -> FunType (IType TypeUnit:| []) tyE
           (p:ps) ->  FunType (NE.map patType (p:|ps)) tyE
      
      return (gammaR'', LamE pats' expr', FType funty, imports' )

  {-
      Delta, Gamma |- state : S     Delta, Gamma |- method : S -> Tm
  ========================================================================
                Delta, Gamma |- Bind state method : Tm
  -}
  (StateFunE stateVar (MethodUnres methodB) args) -> do
    -- We need to get the state type before the method type, because the type of the method depends on the
    -- type of the state i.e. obj.clone() -->  Arc::clone ? String::clone ? 
    (gamma', stateVar', stateTy, imports') <- typeExpr delta imports gamma stateVar

    let maybeMethodNS = case stateTy of
            HType hty _  -> toPath hty
            _ -> Nothing 

    -- Now we need to add the name of the state type to the namespace of the method
    methodQB <- case maybeMethodNS of 
      Just method_ns -> addStateToNS method_ns methodB
      Nothing -> return (QualifiedBinding (NSRef []) methodB)

   -- FIXME: Ho to properly construct an unresolved stateful function (type)?
    (gamma'', methodE , methodTy, imports'') <- typeExpr delta imports' gamma (LitE (FunRefLit (FunRef methodQB Nothing (STFunType TStar [] TStar))))
    
    (fty, ty) <- case methodTy of
            -- Question: Why don't we do the partial application type check here?
            FType fty@(STFunType sTy _ resTy) | heq sTy stateTy -> return (fty, resTy)
            FType (STFunType sTy _ resTy) -> typeError $ "State types "<> show sTy <>" and "<> show stateTy <> " do not match."
            _ -> typeError $ "Method type "<> show methodTy <>" is not a stateful function type."
    
    
    (gammaRs, args', argTypesR) <- 
      (unzip4 <$> mapM (typeExpr delta imports'' gamma) args) 
      >>= (\ (gammaR, argsT, argTy , _ ) -> return (gammaR, argsT, argTy))

    -- Now, as in function application, merge the gammas commming from typing subexpressions to compare
    -- (or typescheck respectively) the types in surrounding expressions

    let gamma_merged = foldl (\gs g -> HM.union gs g) gamma' gammaRs
    -- ToDO: ArgTys should match function tys
    -- traceM $ "Gamma after typing Statefu function " <> show gamma_merged

    return (gamma_merged, StateFunE stateVar' (MethodRes methodQB fty) args', ty, imports'')

  {-
      Delta, Gamma |- cond : Bool    Delta, Gamma |- tTrue : T   Delta, Gamma |- tFalse : T
  ===========================================================================================
                  Delta, Gamma |- If cond tTrue tFalse : T
  -}
  (IfE cond tTrue tFalse) -> do
    (_, cond', condTy, imports') <- typeExpr delta imports gamma cond
    let is_bool = case condTy of 
            IType TypeBool -> True
            HType hTy _ -> canbeBool hTy
            _ -> False
    if is_bool
    then return ()
    else typeError$ "Condition input does not have type bool BUT " <> show condTy

    (gammaT, tTrue', tTrueTy, imports'') <- typeExpr delta imports' gamma tTrue
    (gammaF, tFalse', tFalseTy, imports''' ) <- typeExpr delta imports'' gamma tFalse
    if (heq tTrueTy tFalseTy)
    then return ()
    else typeError "Conditional branches have different types."
    let gamma_merged = HM.union gammaT gammaF
    return (gamma_merged, IfE cond' tTrue' tFalse', tTrueTy, imports''')

  {-
   Delta, Gamma |- cond : Bool       Delta, Gamma |- body : T
  ===============================================================
            Delta, Gamma |- While cond body : Unit

  -}
  (WhileE cond body) -> do
    (_, cond', condTy, imports' ) <- typeSystem delta imports gamma cond

    let is_bool = case condTy of 
          IType TypeBool -> True
          HType hTy _ -> canbeBool hTy
          _ -> False
    if is_bool
    then return ()
    else typeError$ "Condition input for while loop does not have type bool BUT " <> show condTy

    (gamma', body', _bodyTy, imports'' ) <- typeExpr delta imports' gamma body

    return (gamma', WhileE cond' body', IType TypeUnit, imports'')

  {-
      Delta, Gamma |- generator : T1<T2>     Delta, Gamma, x:T2 |- loopFun : T3
  ===============================================================================
          Delta, Gamma |- MapE loopFun generator : T1<T3>
  -}
  (MapE loopFun gen) ->  do
    (_, gen', genTy, imports') <- typeExpr delta imports gamma gen
    
    -- FIXME: We cannot type or typecheck elelments from the generator based on it's type
    --        because we don't know how iteration is implemented for arbitrary generator types

    let mElemTy = case genTy of
          (HType listTy _) -> (asListElementType listTy)
          _ -> Nothing

    eTy <- case mElemTy of
                Just ty -> return ty
                Nothing -> typeError $ "Loop generator is not a list but: " <> show genTy
    
    -- The statement <for i in something> is a repeated assigment of i, to pass the type information
    -- of i to the loop body expression, which will be <Lam i -> actual loop body> we annotate the
    -- pattern i here  
    let loopFun' = case loopFun of
          LamE pats body -> LamE (annotate pats eTy) body
          -- FIXME: I don't think this should ever happen  
          otherExpr -> otherExpr

    (gamma', loopFun', loopFunTy, imports'' ) <- typeExpr delta imports' gamma loopFun'

    return (gamma', MapE loopFun' gen', IType $ TypeList loopFunTy, imports'')
    where 
      annotate [VarP i ty] elemTy = [VarP i (HType elemTy Nothing)]
      annotate [TupP pats] elemTy = annotate_r (NE.toList pats) (asHtypes elemTy)
      annotate pats elemTy = annotate_r pats (asHtypes elemTy) 

      annotate_r (VarP i _ty: pats) (et:etys) = VarP i et : annotate_r pats etys
      annotate_r pats@(TupP _:_) _tys = error $ "Found a nested tuple pattern in a Loop expression."
              <> show pats
              <>" Please don't nest patterns (currently)" 
      annotate_r [] [] = []
      annotate_r pats tys = error $ "Length of patterns " <> show pats  <> " does not match available types " <> show tys <> " in loop header."

      asHtypes elemTy = map (flip HType Nothing) (NE.toList $ unTupleType elemTy)

  {- 
      Delta, Gamma |- e1:T1    Delta, Gamma |- cont : T2
  =======================================================
          Delta, Gamma |- Stmt e1 cont : T2
  -}
  (StmtE e1 cont) -> do
    -- traceM $ "Gonna type a Stmt with Gamma " <> show gamma 
    (gammaR, e1', _e1Ty, imports') <- typeExpr delta imports gamma e1
    -- traceM $ "Gamma after first expression in Statement" <> show gammaR
    (gammaR', cont', contTy, imports'' ) <- typeExpr delta imports' gamma cont
    -- traceM $ "Gamma after second expression in Statement" <> show gammaR'
    let gamma_merge = HM.union gammaR gammaR' 
    return (gamma_merge, StmtE e1' cont', contTy, imports'' )

  {-
      Delta, Gamma |- e1:T1  Delta, Gamma |- e2:T2   ...   Delta, Gamma |- en: Tn
  ======================================================================================
        Delta, Gamma |- TupE [e1, e2 ... , en] : TupleTy [T1, T2, .. , Tn]
  -}
  (TupE exprs) -> do
    -- FIXME: How to handle imports here?
    (gamma' :| _, exprs',exprsTy, importss') <- neUnzip4 <$> mapM (typeExpr delta imports gamma) exprs
    let imports' = nub . concat $ NE.toList importss'
    return (gamma', TupE exprs', TType exprsTy, imports')

  {-
       x:T in Gamma
  ========================
    Delta, Gamma |– x: T
  -}
  (VarE bnd ty) ->  handleVar gamma bnd ty
  (LitE (EnvRefLit bnd ty)) -> (\(g, _, ty', imports') -> (g, LitE $ EnvRefLit bnd ty', ty', imports')) <$> handleVar gamma bnd ty

  {-
     l:T in Delta
  ========================
    Delta, Gamma |- l : T
  -}
  (LitE (FunRefLit (FunRef qBnd@(QualifiedBinding ns bnd) id ty))) -> do
    -- Currently we get function literals mostly/only from the context of method calls, because when we translate (pure) call expressions
    -- the call can be different things (closures, variables, list indices ...) and will mostly be a variable
    -- So the qualified binding should contain the object type the method is called on and we need to do a name resolution i.e. cannot
    -- expect to be e.g. Arc::new() in the context directly, instead there will be std::Arc::new().
    -- ToDo: We have a problem here.
        --  1. The python integration does not extract function types and delta is always empty and
        --  2. Using delta we will get conflicts with generic functions i.e. f<G>(i:G) -> G with G being i32 or String or ... will be in the same delta?!
        -- either we give local type assignment precedence, and/or accept a lookup miss in Delta, and/or put functions including call side identifiers the
        -- for the last option we'd also need to able to identify generics in HostTypes to know when alternatives are valid
        -- I'll go with accepting lookup miss first if we can convert ty to a resolved function type
      case unresToRes (FType ty) of
          Just ty'@(FType fty)  -> return (HM.empty, LitE (FunRefLit (FunRef qBnd id fty)), ty', imports)
          _ -> do 
            (g, e, t, i) <- handleRef bnd (Just ns) (FType ty)
            return (g,e,t,i)

  {-
  ==================
    Gamma |- l : HostType
  -}
  (LitE (NumericLit n)) -> return (HM.empty, LitE $ NumericLit n, IType TypeNat, imports) -- FIXME incorrect. we should not have this in this language!
  (LitE (BoolLit b))    -> return (HM.empty, LitE $ BoolLit b   , IType TypeBool, imports)
  (LitE UnitLit)        -> return (HM.empty, LitE UnitLit       , IType TypeUnit, imports)
  (LitE (StringLit s))  -> return (HM.empty, LitE $ StringLit s , IType TypeString, imports)
  where
    -- When we encounter a variable, we first try to get it's type from 
    -- the local context Gamma. 
    -- If we can't find it's name there, we check if it's a reference to the global (function type) context Delta
    -- passing it to handleRef
    handleVar gamma bnd ty = do
      case HM.lookup bnd gamma of
        Just ty1 ->
          case unresToRes ty1 of
            Just ty1' -> (\ty' -> (HM.singleton bnd ty', VarE bnd ty', ty', imports)) <$> maxType ty ty1'
            Nothing -> handleRef bnd Nothing ty
        _ -> handleRef bnd Nothing ty

    handleRef bnd nSpace ty = do 
      -- We might get references like std::sync::Arc::new here. 
      -- Such references i.e. constructor functions with namespace bring that namespace into scope i.e.
      -- iff there's a namespace on a function (std::sync::Arc) AND the return type of the function corresponds to the last
      -- element of that namespace (new()-> Arc) then this actually is like a full import of the namespace (std::sync::Arc)
      -- and we need to add it to the imports. Full import means std::sync::Arc, as oposed to a global import std::sync::Arc::*;
      -- traceM ("Resolving symbol " <> show bnd <> " with nSpace " <> show nSpace)
      case resolveSymbols delta imports nSpace bnd of 
          Left (qBnd,ty1) -> do 
                new_ty <- maxType ty (FType ty1)
                -- now check if the function has a namespace
                let imports' = case nSpace of
                      -- now check if that namespace is "imported", because the function returns the last part of it
                      Just (NSRef spaces)  -> 
                        case getReturnType new_ty of 
                          Just (HType hTy _) | (Just (last spaces)) == (getBinding . toPath $ hTy) -> imports ++ [Full (NSRef $ init spaces) (last spaces)]
                          _ -> imports
                      Nothing -> imports
                return  (HM.empty, LitE $ FunRefLit $ FunRef qBnd Nothing ty1, new_ty, imports')

          Right (BndError b) -> symResError $ "Unresolved symbol: " <> quickRender b
          Right (QBndError qb) -> symResError $ "Unresolved qualified symbol: " <> quickRender qb
          Right (NoTypeFound qb) -> wfError $ "No type in environment found for qualified symbol: " <> quickRender qb
          Right (Ambiguity qb1 qb2) -> symResError $ "Symbol ambiguity detected.\n" <> quickRender qb1 <> "\n vs.\n" <> quickRender qb2
          Right (AmbiguousImports qbs) -> 
              symResError $ "Symbol import ambiguity detected.\n" <> foldl (\str pot_import -> str <> quickRender pot_import <> ",\n ") "" qbs


addStateToNS ::(ErrAndLogM m, TypeErrorM m ty) => Either Binding QualifiedBinding -> Binding -> m QualifiedBinding
addStateToNS stateTyBnd  bnd = do
    let new_ns = case stateTyBnd of 
            -- This will give a path like std::something ++ Arc
            Left sbnd -> NSRef [sbnd]
            Right (QualifiedBinding (NSRef sbnds) sbnd) -> NSRef (sbnds ++ [sbnd])
    return (QualifiedBinding new_ns bnd)
addStateToNS _ e = throwErrorWithTrace $ 
  "The compiler didn't expect a method to be anything else than a function literal but it was:\n " 
  <> show e 
  <> "\nThis might still be legitimate so please file a bug."

typePat :: (ErrAndLogM m, TypeErrorM m ty) => UnresolvedPat ty -> OhuaType ty Resolved -> m (ResolvedPat ty)
typePat pat newTy = do
  let oldTy = patType pat
  newTy' <- maxType oldTy newTy
  go pat newTy'
  where
    go (VarP bnd _) t = return $ VarP bnd t
    go (TupP (p:|ps)) (TType (pTy:|psTy)) = (\x xs -> TupP $ x:|xs) <$> go p pTy <*> (mapM (uncurry go) $ zip ps psTy)
    go (TupP _) _ = invariantBroken "Tuple pattern with zero sub-patterns encountered. Please file a bug"

maxType :: (ErrAndLogM m, TypeErrorM m ty) => OhuaType ty Unresolved -> OhuaType ty Resolved -> m (OhuaType ty Resolved)
-- maxType TypeNat TypeNat = return TypeNat
-- maxType TypeBool TypeBool = return TypeBool
-- maxType TypeUnit TypeUnit = return TypeUnit
-- maxType TypeString TypeString = return TypeString
-- maxType (TypeList x) (TypeList y) = TypeList <$> maxType x y
maxType (HType t1 _) (HType t2 _) | t1 == t2 = return $ HType t2 Nothing
-- ^ unequal host types -> for know thats an error, but actually we need to resort to Rust here e.g Self vs ActualType => ActuaType
maxType (HType t1 _) (HType t2 _) = typeError $ "Comparing types " <> show t1 <> " and " <> show t2 <> " failed."
maxType (TType (x:|xs)) (TType (y:|ys)) =
   if length xs == length ys
   then do
     xs' <- mapM (uncurry maxType) $ zip xs ys
     x' <- maxType x y
     return $ TType (x':|xs')
   else throwError "Type error: list with different length detected."
maxType (FType fty1) (FType fty2) = do 
    mTy <- maxFunType fty1 fty2
    return (FType mTy)
maxType TStar t2 = return t2
maxType UType (IType TypeUnit) = return (IType TypeUnit)
maxType UType t = typeError $ "Comparing incompatible types:\n UType \n and: \n " <> show t
maxType t1@(HType _ _)  t2@(TType _)        = typeError $ "Comparing incompatible types:\n " <> show t1 <> "\n and: \n " <> show t2
maxType t1@(HType hty _)  t2@(IType TypeUnit) 
    | isHostUnit hty = return (HType hty Nothing)
    | otherwise =  typeError $ "Comparing incompatible types:\n " <> show t1 <> "\n and: \n " <> show t2
maxType t1@(HType _ _)  t2@(IType _)        = typeError $ "Comparing incompatible types:\n " <> show t1 <> "\n and: \n " <> show t2

maxType t1@(TType tys)  t2@(HType ht _ )  
   | length (unTupleType ht) == length tys = 
      let loweredTuple = (NE.map (flip HType Nothing) (unTupleType ht)) -- The extracted Type was a hosttype representation of a tuple type with the same number of types as in the unresolved Tuple Type
      in return (TType loweredTuple)
   | otherwise = typeError $ "Comparing incompatible types:\n " <> show t1 <> "\n and: \n " <> show t2 <> "\n Untupled hosttype to " <> show (unTupleType ht) 
maxType t1@(TType _)    t2@(IType _)        = typeError $ "Comparing incompatible types:\n " <> show t1 <> "\n and: \n " <> show t2
maxType t1@(FType _)    t2                  = typeError $ "Comparing incompatible types:\n " <> show t1 <> "\n and: \n " <> show t2
maxType t1              t2@(FType _)        = typeError $ "Comparing incompatible types:\n " <> show t1 <> "\n and: \n " <> show t2


maxFunType :: (ErrAndLogM m, TypeErrorM m ty) => FunType ty Unresolved -> FunType ty Resolved -> m (FunType ty Resolved)
maxFunType (FunType ins out) (FunType rIns rout) = do
  -- If we didn't know the function type, the length of input types given and resolved will not match
  -- On the other hand, if we knew the function type we want to compare each input type with the resolved type
  -- So we check, if input types are convertible to resolved types and if so compare them to our resolution
  -- Otherwise we just take the resolution

  max_intypes <- case mapM unresToRes ins of
             Nothing -> return rIns
             (Just resolvedIns) 
                  | length resolvedIns == 0 -> return rIns
                  | length resolvedIns == length rIns -> mapM (uncurry maxType) (NE.zip ins rIns)
                  | otherwise -> typeError $ "Comparing given and extracted function input types yielded unequal number of arguments in " 
                                                    <> show ins <> " vs " <> show rIns
             
  FunType max_intypes <$> maxType out rout

-- maxFunType (STFunType sIn ins out) (STFunType rsIn rIns rout) =
--     STFunType <$> maxType sIn rsIn <*> mapM (uncurry maxType) (zip ins rIns) <*> maxType out rout
maxFunType (STFunType sIn ins out) (STFunType rsIn rIns rout) = do 
  max_intypes <- case mapM unresToRes ins of
             Nothing -> return rIns
             (Just resolvedIns) 
                  | length resolvedIns == 0 -> return rIns
                  | length resolvedIns == length rIns -> mapM (uncurry maxType) (zip ins rIns)
                  | otherwise -> typeError $ "Comparing given and extracted function input types yielded unequal number of arguments in " 
                                                    <> show ins <> " vs " <> show rIns
             
  STFunType <$> maxType sIn rsIn <*> pure max_intypes <*> maxType out rout
maxFunType fun otherfun = typeError $ "Comparing stateful to stateless function type " <> show fun <> " with " <> show otherfun


getBinding:: Maybe (Either Binding QualifiedBinding) -> Maybe Binding
getBinding = \case
    Nothing -> Nothing 
    Just (Left bnd) -> Just bnd
    Just (Right (QualifiedBinding _ bnd)) -> Just bnd

--FIXME: Remove/Replace this when we have the ability to actually copare host types in terms of subtyping and generics
compromise_compare :: OhuaType ty Resolved -> OhuaType ty Resolved -> Bool
compromise_compare (HType _ _ ) (HType _ _) = True
-- We need to distiguish unit funtions (i.e. with an added unit argument) from actual one argument functions
-- Also we have the problem, that literals get internal types currently, which cannot be compared to HostTypes
compromise_compare (HType _ _ ) (IType iTy) 
    | iTy == TypeUnit = False
    | otherwise = True
compromise_compare (IType iTy) (HType _ _ ) 
    | iTy == TypeUnit = False
    | otherwise = True
compromise_compare (TType _  )  (TType _) = True
compromise_compare (FType _ )   (FType _) = True
compromise_compare (IType _ )   (IType _) = True
compromise_compare t1 t2                    = False
