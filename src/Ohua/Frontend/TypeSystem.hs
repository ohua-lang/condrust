{-# LANGUAGE ScopedTypeVariables #-}

module Ohua.Frontend.TypeSystem
  (Delta(..), toWellTyped)
where


import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

import Ohua.UResPrelude hiding (Nat)
import qualified Ohua.Prelude as Res
  ( Lit(..), FunType(..), FunRef(..))

import Ohua.PPrint

import Ohua.Frontend.PPrint (prettyExpr)
import Ohua.Frontend.Lang
    ( UnresolvedExpr(..)
    , ResolvedExpr(..)
    , Expr(..)
    , UnresolvedPat(..)
    , ResolvedPat(..)
    , UnresolvedType
    , ResolvedType
    , Pat(TupP, VarP)
    , patTyBnds
    , patType
    , freeVars
    , unitParams
    )
  
import Ohua.Frontend.SymbolResolution (SymResError(..), Delta, Gamma(..), resolveSymbols)

import Ohua.Types.Vector (Nat(..))

import Control.Exception (assert, throw)


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

type Delta' ty res = ([Import], Delta ty res)

toWellTyped :: forall ty m. ErrAndLogM m => Delta ty 'Resolved -> [Import] -> UnresolvedExpr ty -> m (ResolvedExpr ty)
toWellTyped delta imports e =
  let
    gamma = HM.fromList $ freeVars e
  in do
    traceM "delta (pretty):"
    traceM $ renderStrict $ layoutSmart defaultLayoutOptions $ pretty $ HM.toList delta
    traceM ""
    (_gamma', e', _ty) <- flip runReaderT (e :| []) $ typeSystem (imports, delta) gamma e
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
            => Delta' ty Resolved
            -> Gamma ty Unresolved
            -> UnresolvedExpr ty
            -> m (Gamma ty Resolved , ResolvedExpr ty, OhuaType ty Resolved)
typeExpr delta gamma e = local ( <> (e:|[])) $ typeSystem delta gamma e



typeSystem :: forall ty m
           .  (ErrAndLogM m, TypeErrorM m ty)
           => Delta' ty Resolved
           -> Gamma ty Unresolved
           -> UnresolvedExpr ty
           -> m (Gamma ty Resolved, ResolvedExpr ty, OhuaType ty Resolved)
typeSystem delta gamma = \case
  {-
    Delta, Gamma |– e1: T1     Delta, Gamma, x:(max X T1) |– e2: T2
  ==================================================================
             Delta, Gamma |– let (x:X) e1 e2 : T2
  -}
  (LetE pat e1 e2) -> do
    (_, e1', tyT1') <- typeExpr delta gamma e1
    pat' <- typePat pat tyT1'
    -- ToDo: We currently can't allways get unresolved types from resolved ones alothogh I think this should be possible.
    let mResTypes = mapM 
          (\(bnd, tyR) ->  case resToUnres tyR of 
              Just tyU -> Just (bnd, tyU)
              Nothing -> Nothing)
          $ patTyBnds pat'
    -- let patTys = mapM (second resToUnres) $ patTyBnds pat'
    let gamma' = case mResTypes of 
            Just tys -> foldl (\ g (b, t) -> HM.insert b t g) gamma tys
            Nothing -> error $ "Implementation Error. I should be probably be possible to convert " <> show mResTypes <> " to unresolved tpyes but we didn't implement it"

    (gamma'', e2', tyT2) <- typeExpr delta gamma' e2

    return (gamma'', LetE pat' e1' e2', tyT2)

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
      assocArgWithType (t:ts) (arg:args) = do
        (argsAndTy, pendingTy) <- assocArgWithType ts args
        return ((arg,t) : argsAndTy, pendingTy)
    in do
      (gamma', fun', funTy) <- typeExpr delta gamma fun

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
        [] -> invariantBroken $ "Applications need to have at least one argument. The function " <> show fun <> " did not. This is a compiler error. Please file a bug"
        (a:as) -> (neUnzip3 <$> mapM (typeExpr delta gamma) (a:|as)) >>= (\ (_, argsT, _ ) -> return argsT)
      -- Question: Shouldn't we check derived argTys against function input types?
      -- (_, args', _argsTy) <- neUnzip3 <$> mapM (typeExpr delta gamma) args

      return (gamma', AppE fun' args', resTy)

  {-
              Delta, Gamma, p1:T1, p2:T2, ..., pn:Tn |- e:Te
  =================================================================
      Delta, Gamma |- Lambda p1 p2 .. pn. e : T1 -> T2 -> ... -> Tn -> Te
  -}
  e@(LamEU pats expr) ->
    let
      invariantGetGamma :: Binding -> StateT (Gamma ty Resolved) m (OhuaType ty Resolved)
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
      (gamma'', expr', tyE) <- typeExpr delta gamma' expr
      (pats', gamma''') <- runStateT (mapM typePatFromGamma patsNE) gamma''
      return (pats', gamma''', tyE, expr')
      
      let ty = FunType (NE.map patType pats') tyE

      return (gamma''', LamE pats' expr', FType ty)
  {-
      Delta, Gamma |- state : S     Delta, Gamma |- method : S -> Tm
  ========================================================================
                Delta, Gamma |- Bind state method : Tm
  -}
  (BindE method stateB args) -> do
    -- (gamma',  state' , stateTy ) <- typeExpr delta gamma state
    (gamma', method', methodTy) <- typeExpr delta gamma method
    -- TODO get type prefix from stateTy via Pathable ????
    let qb = QualifiedBinding (makeThrow []) stateB
    -- ToDo: Should use the qualified binding
    (_gamma, _stateVar, stateTy) <- handleVar gamma stateB TStar
    
    ty <- case methodTy of
            -- Question: Why don't we do the partial application type check here?
            FType (STFunType sTy _ resTy) | heq sTy stateTy -> return resTy
            _ -> typeError $ "State types do not match."
    -- if args are empty, we need to pass an explicit unit argument
    args' <- case args of
      [] -> return (LitE UnitLit:|[]) 
      (a:as) -> (neUnzip3 <$> mapM (typeExpr delta gamma) (a:| as)) >>= (\ (_, argsT, _ ) -> return argsT)

    -- gamma doesn't change (at least in the current system) as args are only usage sites
    --(_, args', _argsTy) <- neUnzip3 <$> mapM (typeExpr delta gamma) args
    -- ToDO: ArgTys should match function tys

    return (gamma', StateFunE (VarE stateB stateTy) qb method', ty)

  {-
      Delta, Gamma |- cond : Bool    Delta, Gamma |- tTrue : T   Delta, Gamma |- tFalse : T
  ===========================================================================================
                  Delta, Gamma |- If cond tTrue tFalse : T
  -}
  (IfE cond tTrue tFalse) -> do
    (_, cond', condTy) <- typeExpr delta gamma cond
    if (heq condTy ((IType TypeBool)::OhuaType ty Resolved))
    then return ()
    else typeError "Condition input does not have type bool."

    (_, tTrue', tTrueTy) <- typeExpr delta gamma tTrue
    (gamma', tFalse', tFalseTy) <- typeExpr delta gamma tFalse
    if (heq tTrueTy tFalseTy)
    then return ()
    else typeError "Conditional branches have different types."

    return (gamma', IfE cond' tTrue' tFalse', tTrueTy)

  {-
   Delta, Gamma |- cond : Bool       Delta, Gamma |- body : T
  ===============================================================
            Delta, Gamma |- While cond body : Unit

  -}
  (WhileE cond body) -> do
    (_, cond', condTy) <- typeSystem delta gamma cond
    if (heq condTy (IType TypeBool:: OhuaType ty Resolved))
    then return ()
    else typeError "Condition input for while loop does not have type bool."

    (gamma', body', _bodyTy) <- typeExpr delta gamma body

    return (gamma', WhileE cond' body', IType TypeUnit)

  {-
      Delta, Gamma |- generator : T1<T2>     Delta, Gamma, x:T2 |- loopFun : T3
  ===============================================================================
          Delta, Gamma |- MapE loopFun generator : T1<T3>
  -}
  (MapE loopFun gen) ->  do
    (_, gen', genTy) <- typeExpr delta gamma gen
    elemTy <- case genTy of
                IType (TypeList eTy) -> return eTy
                _ -> typeError "Loop generator is not a list!"

    (gamma', loopFun', loopFunTy) <- typeExpr delta gamma loopFun

    return (gamma', MapE loopFun' gen', IType $ TypeList loopFunTy)

  {-
      Delta, Gamma |- e1:T1    Delta, Gamma |- cont : T2
  =======================================================
          Delta, Gamma |- Stmt e1 cont : T2
  -}
  (StmtE e1 cont) -> do
    (_, e1', _e1Ty) <- typeExpr delta gamma e1
    (gamma', cont', contTy) <- typeExpr delta gamma cont
    return (gamma', StmtE e1' cont', contTy)

  {-
      Delta, Gamma |- e1:T1  Delta, Gamma |- e2:T2   ...   Delta, Gamma |- en: Tn
  ======================================================================================
        Delta, Gamma |- TupE [e1, e2 ... , en] : TupleTy [T1, T2, .. , Tn]
  -}
  (TupE exprs) -> do
    (gamma' :| _, exprs',exprsTy ) <- neUnzip3 <$> mapM (typeExpr delta gamma) exprs
    return (gamma', TupE exprs', TType exprsTy)

  {-
       x:T in Gamma
  ========================
    Delta, Gamma |– x: T
  -}
  (VarE bnd ty) -> handleVar gamma bnd ty
  (LitE (EnvRefLit bnd ty)) -> (\(g, _, ty') -> (g, LitE $ EnvRefLit bnd ty', ty')) <$> handleVar gamma bnd ty

  {-
     l:T in Delta
  ========================
    Delta, Gamma |- l : T
  -}
  (LitE (FunRefLit (FunRef qbnd id ty))) -> do
      let (imports,delta') = delta
      case HM.lookup qbnd delta' of
        Just ty' ->
          ((\ty'' ->
            (HM.empty, LitE $ FunRefLit $ FunRef qbnd id ty', ty'')) .
          FType) <$> maxFunType ty ty'
        Nothing -> typeError $ "Missing type information for function literal: " <> (quickRender qbnd)

  {-
  ==================
    Gamma |- l : HostType
  -}
  (LitE (NumericLit n)) -> return (HM.empty, LitE $ NumericLit n, IType TypeNat) -- FIXME incorrect. we should not have this in this language!
  (LitE (BoolLit b))    -> return (HM.empty, LitE $ BoolLit b   , IType TypeBool)
  (LitE UnitLit)        -> return (HM.empty, LitE UnitLit       , IType TypeUnit)
  (LitE (StringLit s))  -> return (HM.empty, LitE $ StringLit s , IType TypeString)
  where
    handleRef bnd ty = do 
      let (imports,delta') = delta
      case resolveSymbols delta' imports Nothing bnd of 
          Left (qBnd,ty1) ->
                          (\ty' ->
                            (HM.empty, LitE $ FunRefLit $ FunRef qBnd Nothing ty1, ty'))
                          <$> maxType ty (FType  ty1)
          Right (BndError b) -> symResError $ "Unresolved symbol: " <> quickRender b
          Right (QBndError qb) -> symResError $ "Unresolved qualified symbol: " <> quickRender qb
          Right (NoTypeFound qb) -> wfError $ "No type in environment found for qualified symbol: " <> quickRender qb
          Right (Ambiguity qb1 qb2) -> symResError $ "Symbol ambiguity detected.\n" <> quickRender qb1 <> "\n vs.\n" <> quickRender qb2

    handleVar gamma bnd ty = do
      case HM.lookup bnd gamma of
        Just ty1 ->
          case unresToRes ty1 of
            Just ty1' -> (\ty' -> (HM.singleton bnd ty', VarE bnd ty', ty')) <$> maxType ty ty1'
            Nothing -> handleRef bnd ty
        _ -> handleRef bnd ty

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
maxType t1@(HType t _)  t2@(IType _)        = typeError $ "Comparing in compatible types " <> show t1 <> " and " <> show t2
maxType t1@(TType _)    t2@(HType _ _ )     = typeError $ "Comparing in compatible types " <> show t1 <> " and " <> show t2
maxType t1@(TType _)    t2@(IType _)        = typeError $ "Comparing in compatible types " <> show t1 <> " and " <> show t2
maxType t1@(FType _)    t2                  = typeError $ "Comparing in compatible types " <> show t1 <> " and " <> show t2
maxType t1              t2@(FType _)        = typeError $ "Comparing in compatible types " <> show t1 <> " and " <> show t2


maxFunType :: (ErrAndLogM m, TypeErrorM m ty) => FunType ty Unresolved -> FunType ty Resolved-> m (FunType ty Resolved)
maxFunType (FunType ins out) (FunType rins rout) =
  FunType <$> mapM (uncurry maxType) (NE.zip ins rins) <*> maxType out rout
maxFunType (STFunType sin ins out) (STFunType rsin rins rout) =
  STFunType <$> maxType sin rsin <*> mapM (uncurry maxType) (zip ins rins) <*> maxType out rout
maxFunType fun otherfun = typeError $ "Comparing stateful to stateless function type " <> show fun <> " with " <> show otherfun
