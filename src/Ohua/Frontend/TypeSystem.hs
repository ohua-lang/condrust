{-# LANGUAGE ScopedTypeVariables #-}

module Ohua.Frontend.TypeSystem
  (Delta, toWellTyped)
where


import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE 
import Data.List (nub, last)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

import Ohua.Prelude hiding (Nat, last)
import qualified Ohua.Prelude as Res
  ( Lit(..), FunType(..), FunRef(..))

import Ohua.Frontend.PPrint (prettyExpr)
import Ohua.Frontend.Lang
    ( UnresolvedExpr
    , ResolvedExpr
    , Expr(..)
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
toWellTyped delta modImports e =
  let
    gamma = HM.fromList $ freeVars e
  in do
    {-
    traceM "delta (pretty):"
    traceM $ renderStrict $ layoutSmart defaultLayoutOptions $ pretty $ HM.toList delta
    traceM ""
    -}
    (_gamma', e', _ty, imports') <- flip runReaderT (e :| []) $ typeSystem  delta modImports gamma e
    return e'

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
    (_, e1', tyT1', imports' ) <- typeExpr delta imports gamma e1
    pat' <- typePat pat tyT1'
    -- ToDo: We currently can't allways get unresolved types from resolved ones although I think this should be possible.
    let mResTypes = mapM 
          (\(bnd, tyR) ->  case resToUnres tyR of 
              Just tyU -> Just (bnd, tyU)
              Nothing -> Nothing)
          $ patTyBnds pat'
    -- let patTys = mapM (second resToUnres) $ patTyBnds pat'
    let gamma' = case mResTypes of 
            Just tys -> foldl (\ g (b, t) -> HM.insert b t g) gamma tys 
            Nothing -> error $ "Implementation Error. I should be probably be possible to convert " <> show mResTypes <> " to unresolved tpyes but we didn't implement it"

    (gamma'', e2', tyT2, imports'' ) <- typeExpr delta imports' gamma' e2

    return (gamma'', LetE pat' e1' e2', tyT2, imports'' )

  {-
               Delta, Gamma |– fun: T1 -> T2 -> ... Tm -> Tf  n<=m
     Delta, Gamma |-t1: T1  Delta, Gamma |- t2:T2  ...  Delta, Gamma |- tn:Tn
  ===================================================================================
                  Delta, Gamma |– fun t1 t2 ... tn : Tf
  -}
  (AppEU fun args) ->
    let
      -- This accounts for partial application i.e. if the number of args
      -- matches the number of types in the declation the return type is the return type of the 
      -- function ... otherwise it returns a partially applied function
      assocArgWithType [] (_:_) =
        wfError "Too many arguments in function application."
      -- Question: How is it ok to have more argTypes than args?
      assocArgWithType l [] = return ([], l)
      assocArgWithType (t:ts) (arg:args') = do
        (argsAndTy, pendingTy) <- assocArgWithType ts args'
        return ((arg,t) : argsAndTy, pendingTy)
    in do
      (gamma', fun', funTy, imports' ) <- typeExpr delta imports gamma fun

      resTy <- case funTy of
        FType (FunType ins out) -> do
            (_, pendingArgsTy) <- assocArgWithType (toList ins) $ toList args
            case pendingArgsTy of
              [] -> return out
              (x:xs)  -> return $ FType  (FunType (x:|xs) out)
        FType (STFunType sin ins out) -> do
            (_, pendingArgsTy) <- assocArgWithType ins $ toList args
            return $ FType  (STFunType sin pendingArgsTy out)
        t -> typeError $ "First argument of function application is not a function, but has type: " <> show t

      -- if args are empty, we need to pass an explicit unit argument
      args' <- case args of
        -- FIXME: Do we expect import changes from args?
        [] -> invariantBroken $ "Applications need to have at least one argument. The function " <> show fun <> " did not. This is a compiler error. Please file a bug"
        (a:as) -> (neUnzip4 <$> mapM (typeExpr delta imports gamma) (a:|as)) >>= (\ (_, argsT, _ , _ ) -> return argsT)
      -- Question: Shouldn't we check derived argTys against function input types?
      -- (_, args', _argsTy) <- neUnzip3 <$> mapM (typeExpr delta imports gamma) args

      return (gamma', AppE fun' args', resTy, imports')

  {-
              Delta, Gamma, p1:T1, p2:T2, ..., pn:Tn |- e:Te
  =================================================================
      Delta, Gamma |- Lambda p1 p2 .. pn. e : T1 -> T2 -> ... -> Tn -> Te
  -}
  e@(LamEU pats expr) ->
    let
      -- FIXME: Replace the check for "_" binding here. We currently introduce it when 
      -- as unit arg representation, but we should have somethign not stringly typed for that purpose 
      invariantGetGamma :: Binding -> StateT (Gamma ty Resolved) m (OhuaType ty Resolved)
      invariantGetGamma bnd | bnd == fromString "_" = return (IType TypeUnit)
      invariantGetGamma bnd = do
        gam <- get
        case HM.lookup bnd gam of
          Nothing -> invariantBroken $ "Pattern deleted while typing. Deleted pattern: " <> show bnd
          Just ty' -> (modify $ HM.delete bnd) >> return ty'

      typePatFromGamma :: UnresolvedPat ty -> StateT (Gamma ty Resolved) m (ResolvedPat ty)
      typePatFromGamma (VarP bnd t) = (\ t' -> VarP bnd <$> maxType t t') =<< invariantGetGamma bnd
      typePatFromGamma (TupP ps) = TupP <$> mapM typePatFromGamma ps
    in do
      patsNE <- case pats of 
            [] -> invariantBroken $ "Abstractions need to have at least one argument. The function " <> show e <> " did not. This is a compiler error. Please file a bug"
            (p:ps) -> return (p:|ps)
      let gamma' = foldl (\ g (b, t) -> HM.insert b t g) gamma $ join $ NE.map patTyBnds patsNE
      (gamma'', expr', tyE, imports' ) <- typeExpr delta imports gamma' expr
      (pats', gamma''') <- runStateT (mapM typePatFromGamma patsNE) gamma''
      --FIXME: What is going on whith this double return here?! 
      return (pats', gamma''', tyE, expr', imports' )
      
      let ty = FunType (NE.map patType pats') tyE

      return (gamma''', LamE pats' expr', FType ty, imports' )

  {-
      Delta, Gamma |- state : S     Delta, Gamma |- method : S -> Tm
  ========================================================================
                Delta, Gamma |- Bind state method : Tm
  -}
  (BindE method stateB args) -> do
    -- We need to get the state type before the method type, because the type of the method depends on the
    -- type of the state i.e. obj.clone() -->  Arc::clone ? String::clone ? 
    let qb = QualifiedBinding (makeThrow []) stateB
    -- ToDo: Should use the qualified binding
    (_gamma, _stateVar, stateTy, imports' ) <- handleVar gamma stateB TStar

    let maybeMethodNS = case stateTy of
            HType hty _  -> toPath hty
            _ -> Nothing 

    -- Now we need to add the name of the state type to the namespace of the method
    method' <- case maybeMethodNS of 
      Just method_ns -> addStateToNS method_ns method
      Nothing -> return method
    traceM $ "Now trying to resolve method " <> show method'

    (gamma', method'', methodTy, imports'' ) <- typeExpr delta imports' gamma method'
    
    
    ty <- case methodTy of
            -- Question: Why don't we do the partial application type check here?
            FType (STFunType sTy _ resTy) | heq sTy stateTy -> return resTy
            _ -> typeError $ "State types do not match."


    -- if args are empty, we need to pass an explicit unit argument
    -- FIXME: That shouldn't happen any more so we should error here. 
    args' <- case args of
      [] -> return (LitE UnitLit:|[]) 
      (a:as) -> (neUnzip4 <$> mapM (typeExpr delta imports'' gamma) (a:| as)) >>= (\ (_, argsT, _ , _ ) -> return argsT)

    -- gamma doesn't change (at least in the current system) as args are only usage sites
    -- ToDO: ArgTys should match function tys

    return (gamma', StateFunE (VarE stateB stateTy) qb method'', ty, imports'' )

  {-
      Delta, Gamma |- cond : Bool    Delta, Gamma |- tTrue : T   Delta, Gamma |- tFalse : T
  ===========================================================================================
                  Delta, Gamma |- If cond tTrue tFalse : T
  -}
  (IfE cond tTrue tFalse) -> do
    (_, cond', condTy, imports') <- typeExpr delta imports gamma cond
    if (heq condTy ((IType TypeBool)::OhuaType ty Resolved))
    then return ()
    else typeError "Condition input does not have type bool."

    (_, tTrue', tTrueTy, imports'') <- typeExpr delta imports' gamma tTrue
    (gamma', tFalse', tFalseTy, imports''' ) <- typeExpr delta imports'' gamma tFalse
    if (heq tTrueTy tFalseTy)
    then return ()
    else typeError "Conditional branches have different types."

    return (gamma', IfE cond' tTrue' tFalse', tTrueTy, imports''')

  {-
   Delta, Gamma |- cond : Bool       Delta, Gamma |- body : T
  ===============================================================
            Delta, Gamma |- While cond body : Unit

  -}
  (WhileE cond body) -> do
    (_, cond', condTy, imports' ) <- typeSystem delta imports gamma cond
    if (heq condTy (IType TypeBool:: OhuaType ty Resolved))
    then return ()
    else typeError "Condition input for while loop does not have type bool."

    (gamma', body', _bodyTy, imports'' ) <- typeExpr delta imports' gamma body

    return (gamma', WhileE cond' body', IType TypeUnit, imports'')

  {-
      Delta, Gamma |- generator : T1<T2>     Delta, Gamma, x:T2 |- loopFun : T3
  ===============================================================================
          Delta, Gamma |- MapE loopFun generator : T1<T3>
  -}
  (MapE loopFun gen) ->  do
    (_, gen', genTy, imports') <- typeExpr delta imports gamma gen
    _elemTy <- case genTy of
                IType (TypeList eTy) -> return eTy
                _ -> typeError "Loop generator is not a list!"

    (gamma', loopFun', loopFunTy, imports'' ) <- typeExpr delta imports' gamma loopFun

    return (gamma', MapE loopFun' gen', IType $ TypeList loopFunTy, imports'')

  {-
      Delta, Gamma |- e1:T1    Delta, Gamma |- cont : T2
  =======================================================
          Delta, Gamma |- Stmt e1 cont : T2
  -}
  (StmtE e1 cont) -> do
    (_, e1', _e1Ty, imports' ) <- typeExpr delta imports gamma e1
    (gamma', cont', contTy, imports'' ) <- typeExpr delta imports' gamma cont
    return (gamma', StmtE e1' cont', contTy, imports'' )

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
      -- element of that namespace (new()-> Arc) then this actually is like an import of the namespace (std::sync::Arc)
      -- and we need to add it to the imports 
      -- traceM ("Resolving symbol " <> show bnd <> " with nSpace " <> show nSpace)
      case resolveSymbols delta imports nSpace bnd of 
          Left (qBnd,ty1) -> do 
                new_ty <- maxType ty (FType ty1)

                -- now check if the function has a namespace
                let imports' = case nSpace of
                      -- now check if that namespace is "imported", because the function returns the last part of it
                      Just (NSRef spaces)  -> 
                        case getReturnType new_ty of 
                          Just (HType hTy _) | (Just (last spaces)) == (getBinding . toPath $ hTy) -> imports ++ [Glob (NSRef spaces)]
                          _ -> imports
                      Nothing -> imports
                return  (HM.empty, LitE $ FunRefLit $ FunRef qBnd Nothing ty1, new_ty, imports')

          Right (BndError b) -> symResError $ "Unresolved symbol: " <> quickRender b
          Right (QBndError qb) -> symResError $ "Unresolved qualified symbol: " <> quickRender qb
          Right (NoTypeFound qb) -> wfError $ "No type in environment found for qualified symbol: " <> quickRender qb
          Right (Ambiguity qb1 qb2) -> symResError $ "Symbol ambiguity detected.\n" <> quickRender qb1 <> "\n vs.\n" <> quickRender qb2

-- ToDo: This is really unelegant i.e. we expect methods to be function literals (or anything else??), 
-- so we shouldn't make this a transformation on all exprs.
addStateToNS ::(ErrAndLogM m, TypeErrorM m ty) => Either Binding QualifiedBinding -> Expr ty 'Unresolved -> m (Expr ty 'Unresolved)
addStateToNS stateTyBnd (LitE (FunRefLit (FunRef (QualifiedBinding (NSRef bnds) bnd) id ty))) = do
    let new_ns = case stateTyBnd of 
            -- This will give a path like std::something ++ Arc
            Left sbnd -> NSRef (bnds ++ [sbnd])
            Right (QualifiedBinding (NSRef sbnds) sbnd) -> NSRef ( bnds ++ sbnds ++ [sbnd])
    return (LitE (FunRefLit (FunRef (QualifiedBinding new_ns bnd) id ty)))
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
maxType UType t = typeError $ "Comparing in compatible types UType and " <> show t
maxType t1@(HType _ _)  t2@(TType _)        = typeError $ "Comparing in compatible types " <> show t1 <> " and " <> show t2
maxType t1@(HType _t _)  t2@(IType _)        = typeError $ "Comparing in compatible types " <> show t1 <> " and " <> show t2
maxType t1@(TType _)    t2@(HType _ _ )     = typeError $ "Comparing in compatible types " <> show t1 <> " and " <> show t2
maxType t1@(TType _)    t2@(IType _)        = typeError $ "Comparing in compatible types " <> show t1 <> " and " <> show t2
maxType t1@(FType _)    t2                  = typeError $ "Comparing in compatible types " <> show t1 <> " and " <> show t2
maxType t1              t2@(FType _)        = typeError $ "Comparing in compatible types " <> show t1 <> " and " <> show t2


maxFunType :: (ErrAndLogM m, TypeErrorM m ty) => FunType ty Unresolved -> FunType ty Resolved-> m (FunType ty Resolved)
maxFunType (FunType ins out) (FunType rins rout) =
  FunType <$> mapM (uncurry maxType) (NE.zip ins rins) <*> maxType out rout
maxFunType (STFunType sIn ins out) (STFunType rsIn rIns rout) =
  STFunType <$> maxType sIn rsIn <*> mapM (uncurry maxType) (zip ins rIns) <*> maxType out rout
maxFunType fun otherfun = typeError $ "Comparing stateful to stateless function type " <> show fun <> " with " <> show otherfun


getBinding:: Maybe (Either Binding QualifiedBinding) -> Maybe Binding
getBinding = \case
    Nothing -> Nothing 
    Just (Left bnd) -> Just bnd
    Just (Right (QualifiedBinding _ bnd)) -> Just bnd