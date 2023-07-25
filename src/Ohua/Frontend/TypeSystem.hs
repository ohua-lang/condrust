{-# LANGUAGE ScopedTypeVariables #-}

module Ohua.Frontend.TypeSystem
  (Delta(..), toWellTyped)
where


import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

import Ohua.UResPrelude hiding (getVarType, Nat)
import qualified Ohua.Prelude as Res
  ( Lit(..), VarType(..), FunType(..), FunRef(..))
import Ohua.Types.Casts
import Ohua.PPrint

import Ohua.Frontend.PPrint (prettyExpr)
import Ohua.Frontend.Lang
    ( UnresolvedExpr
    , ResolvedExpr
    , UnresolvedType
    , ResolvedType
    , Pat(TupP, VarP)
    , patTyBnds
    , patType
    , freeVars
    )
{-
import qualified Ohua.Frontend.WellTyped as WT
    ( Expr(..)
    ,  Pat(TupP, VarP)
    , patTyBnds
    , patType
    )
-}

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

type Gamma varTy ty = HM.HashMap Binding (varTy ty)
type Delta ty = HM.HashMap QualifiedBinding (Res.FunType ty)

type Delta' ty = ([Import], Delta ty)

toWellTyped :: forall ty m. ErrAndLogM m => Delta ty -> [Import] -> UnresolvedExpr ty -> m (ResolvedExpr ty)
toWellTyped delta imports e =
  let
    gamma = HM.fromList $ FrLang.freeVars e
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
            => Delta ty
            -> Gamma UnresolvedType ty
            -> UnresolvedExpr ty
            -> m (Gamma ResolvedType ty, ResolvedExpr ty, ResolvedType ty)
typeExpr delta gamma e = local ( <> (e:|[])) $ typeSystem delta gamma e

typeSystem :: forall ty m
           .  (ErrAndLogM m, TypeErrorM m ty)
           => Delta' ty
           -> Gamma UnresolvedType ty
           -> UnresolvedExpr ty
           -> m (Gamma ResolvedType ty, ResolvedExpr ty, ResolvedType ty)
typeSystem delta gamma = \case
  {-
    Delta, Gamma |– e1: T1     Delta, Gamma, x:(max X T1) |– e2: T2
  ==================================================================
             Delta, Gamma |– let (x:X) e1 e2 : T2
  -}
  (LetE pat e1 e2) -> do
    (_, e1', tyT1') <- typeExpr delta gamma e1
    pat' <- typePat pat tyT1'
    let gamma' = foldl (\ g (b, t) -> HM.insert b t g) gamma
                 $ map (second fromResType)
                 $ WT.patTyBnds pat'
    (gamma'', e2', tyT2) <- typeExpr delta gamma' e2

    return (gamma'', LetE pat' e1' e2', tyT2)

  {-
               Delta, Gamma |– fun: T1 -> T2 -> ... Tm -> Tf  n<=m
     Delta, Gamma |-t1: T1  Delta, Gamma |- t2:T2  ...  Delta, Gamma |- tn:Tn
  ===================================================================================
                  Delta, Gamma |– fun t1 t2 ... tn : Tf
  -}
  (AppE fun args) ->
    let
      assocArgWithType [] (_:_) =
        wfError "Too many argumemts in function application."
      assocArgWithType l [] = return ([], l)
      assocArgWithType (t:ts) (arg:args) = do
        (argsAndTy, pendingTy) <- assocArgWithType ts args
        return ((arg,t) : argsAndTy, pendingTy)
    in do
      (gamma', fun', funTy) <- typeExpr delta gamma fun

      resTy <- case funTy of
        TypeFunction (FunType ins out) -> do
            (_, pendingArgsTy) <- assocArgWithType (toList ins) $ toList args
            case pendingArgsTy of
              [] -> return out
              (x:xs)  -> return $ TypeFunction (FunType (x:|xs) out)
        TypeFunction (STFunType sin ins out) -> do
            (_, pendingArgsTy) <- assocArgWithType ins $ toList args
            return $ TypeFunction (STFunType sin pendingArgsTy out)
        t -> typeError $ "First argument of function application is not a function, but has type: " <> show t

      (_, args', _argsTy) <- neUnzip3 <$> mapM (typeExpr delta gamma) args

      return (gamma', AppE fun' args', resTy)

  {-
              Delta, Gamma, p1:T1, p2:T2, ..., pn:Tn |- e:Te
  =================================================================
      Delta, Gamma |- Lambda p1 p2 .. pn. e : T1 -> T2 -> ... -> Tn -> Te
  -}
  (LamE pats expr) ->
    let
      invariantGetGamma :: Binding -> StateT (Gamma ResolvedType ty) m (ResolvedType ty)
      invariantGetGamma bnd = do
        gam <- get
        case HM.lookup bnd gam of
          Nothing -> invariantBroken $ "Pattern deleted while typing. Deleted pattern: " <> show bnd
          Just ty' -> (modify $ HM.delete bnd) >> return ty'

      typePatFromGamma :: UnresolvedPat ty -> StateT (Gamma ResolvedType ty) m (ResolvedPat ty)
      typePatFromGamma (VarP bnd t) = (\ t' -> VarP bnd <$> maxType t t') =<< invariantGetGamma bnd
      typePatFromGamma (TupP ps) = TupP <$> mapM typePatFromGamma ps
    in do
      let bndsAndTypes = map FrLang.patTyBnds pats
      let gamma' = foldl (\ g (b, t) -> HM.insert b t g) gamma $ neConcat bndsAndTypes
      (gamma'', expr', tyE) <- typeExpr delta gamma' expr
      (pats', gamma''') <- runStateT (mapM typePatFromGamma pats) gamma''
      let ty = TypeFunction $ FunType (map WT.patType pats') tyE

      return (gamma''', LamE pats' expr', ty)
  {-
      Delta, Gamma |- state : S     Delta, Gamma |- method : S -> Tm
  ========================================================================
                Delta, Gamma |- Bind state method : Tm
  -}
  (BindE state f args) -> do
    (gamma',  state' , stateTy ) <- typeExpr delta gamma state
    -- TODO get type prefix from stateTy via Pathable
    let qb = undefined
    (gamma'', method', methodTy) <- typeExpr delta gamma args
    ty <- case methodTy of
            TypeFunction (STFunType sTy _ resTy) | sTy == stateTy -> return resTy
            _ -> typeError $ "State types do not match."

    return (gamma'', StateFunE state' qb method', ty)

  {-
      Delta, Gamma |- cond : Bool    Delta, Gamma |- tTrue : T   Delta, Gamma |- tFalse : T
  ===========================================================================================
                  Delta, Gamma |- If cond tTrue tFalse : T
  -}
  (IfE cond tTrue tFalse) -> do
    (_, cond', condTy) <- typeExpr delta gamma cond
    if condTy == TypeBool
    then return ()
    else typeError "Condition input does not have type bool."

    (_, tTrue', tTrueTy) <- typeExpr delta gamma tTrue
    (gamma', tFalse', tFalseTy) <- typeExpr delta gamma tFalse
    if tTrueTy == tFalseTy
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
    if condTy == TypeBool
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
                TypeList eTy -> return eTy
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
    return (gamma', TupE exprs', IType $ TupleTy exprsTy)

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
      case HM.lookup qbnd delta of
        Just ty' ->
          ((\ty'' ->
            (HM.empty, LitE $ FunRefLit $ FunRef qbnd id ty', ty'')) .
          TypeFunction) <$> maxFunType ty ty'
        Nothing -> typeError $ "Missing type information for function literal: " <> (quickRender qbnd)

  {-
  ==================
    Gamma |- l : HostType
  -}
  (LitE (NumericLit n)) -> return (HM.empty, LitE $ NumericLit n, TypeNat) -- FIXME incorrect. we should not have this in this language!
  (LitE (BoolLit b))    -> return (HM.empty, LitE $ BoolLit b   , TypeBool)
  (LitE UnitLit)        -> return (HM.empty, LitE UnitLit       , TypeUnit)
  (LitE (StringLit s))  -> return (HM.empty, LitE $ StringLit s , TypeString)
  where
    handleRef bnd ty =
      let (imports,delta') = delta
      case resolve delta imports Nothing bnd
        Left (q,ty1) ->
          (\ty' ->
            (HM.empty, LitE $ FunRefLit $ FunRef qbnd Nothing ty1, ty'))
          <$> maxType ty (TypeFunction ty1)
        Right (BndError b) -> symResError $ "Unresolved symbol: " <> quickRender b
        Right (QBndError qb) -> symResError $ "Unresolved qualified symbol: " <> quickRender qb
        Right (NoTypeFound qb) -> wfError $ "No type in environment found for qualified symbol: " <> quickRender qb
        Right (Ambiguity qb1 qb2) -> symResError $ "Symbol ambiguity detected.\n" <> quickRender qb1 <> "\n vs.\n" <> quickRender qb2

    handleVar gamma bnd ty = do
      case HM.lookup bnd gamma of
        Just ty1 ->
          case toResType ty1 of
            Just ty1' -> (\ty' -> (HM.singleton bnd ty', VarE bnd ty', ty')) <$> maxType ty ty1'
            Nothing -> handleRef bnd ty
        _ -> handleRef bnd ty

typePat :: (ErrAndLogM m, TypeErrorM m ty) => UnresolvedPat ty -> ResolvedType ty -> m (ResolvedPat ty)
typePat pat newTy = do
  let oldTy = FrLang.patType pat
  newTy' <- maxType oldTy newTy
  go pat newTy'
  where
    go (VarP bnd _) t = return $ VarP bnd t
    go (TupP (p:|ps)) (TupleTy (pTy:|psTy)) = (\x xs -> TupP $ x:|xs) <$> go p pTy <*> (mapM (uncurry go) $ zip ps psTy)
    go (TupP _) _ = invariantBroken "The impossible happened"

maxType :: (ErrAndLogM m, TypeErrorM m ty) => UnresolvedType ty -> ResolvedType ty -> m (ResolvedType ty)
-- maxType TypeNat TypeNat = return TypeNat
-- maxType TypeBool TypeBool = return TypeBool
-- maxType TypeUnit TypeUnit = return TypeUnit
-- maxType TypeString TypeString = return TypeString
-- maxType (TypeList x) (TypeList y) = TypeList <$> maxType x y
maxType (HType t1 _) (HType t2 _) | t1 == t2 = return $ HType t2
-- ^ unequal host types -> for know thats an error, but actually we need to resort to Rust here e.g Self vs ActualType => ActuaType
maxType (HType t1 _) (HType t2 _) = typeError $ "Comparing types " <> show t1 <> " and " <> show t2 <> " failed."
-- maxType (TupleTy (x:|xs)) (TupleTy (y:|ys)) =
--   if length xs == length ys
--   then do
--     xs' <- mapM (uncurry maxType) $ zip xs ys
--     x' <- maxType x y
--     return $ TupleTy (x':|xs')
--   else throwError "Type error: list with different length detected."
-- maxType (TypeFunction f) (TypeFunction g) = TypeFunction <$> maxFunType f g
maxType TypeStar t2 = return t2

{-
maxFunType :: (ErrAndLogM m, TypeErrorM m ty) => FunType ty Frontend -> FunType ty Resolved-> m (FunType ty Resolved)
maxFunType (FunType ins out) (FunType rins rout) =
  FunType <$> mapM (uncurry maxType) (NE.zip ins rins) <*> maxType out rout
maxFunType (STFunType sin ins out) (STFunType rsin rins rout) =
  STFunType <$> maxType sin rsin <*> mapM (uncurry maxType) (zip ins rins) <*> maxType out rout
-}
