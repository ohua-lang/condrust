{-# LANGUAGE ScopedTypeVariables #-}

module Ohua.Frontend.TypeSystem
  (Delta(..), toWellTyped)
where


import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE

import Ohua.UResPrelude hiding (getVarType)
import qualified Ohua.Prelude as Res
  ( Lit(..), VarType(..), FunType(..), FunRef(..))
import Ohua.Types.Casts

import Ohua.Frontend.PPrint ()
import qualified Ohua.Frontend.Lang as FrLang
    ( Expr(..)
    , Pat(TupP, VarP)
    , patTyBnds
    , patType
    , freeVars
    )
import qualified Ohua.Frontend.WellTyped as WT
    ( Expr(..)
    ,  Pat(TupP, VarP)
    , patTyBnds
    , patType
    )

--import Ohua.Integration.Rust.TypeHandling
--import Ohua.Frontend.TypePropagation

import Control.Exception (assert, throw)

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

toWellTyped :: forall ty m. ErrAndLogM m => Delta ty -> FrLang.Expr ty -> m (WT.Expr ty)
toWellTyped delta e =
  let
    gamma = HM.fromList $ FrLang.freeVars e
  in do
    (_gamma', e, _ty) <- typeSystem delta gamma e
    return e

typeSystem :: forall ty m
           .  ErrAndLogM m
           => Delta ty
           -> Gamma VarType ty
           -> FrLang.Expr ty
           -> m (Gamma Res.VarType ty, WT.Expr ty, Res.VarType ty)
typeSystem delta gamma = \case
  {-
    Delta, Gamma |– e1: T1     Delta, Gamma, x:(max X T1) |– e2: T2
  ==================================================================
             Delta, Gamma |– let (x:X) e1 e2 : T2
  -}
  (FrLang.LetE pat e1 e2) -> do
    (_, e1', tyT1') <- typeSystem delta gamma e1
    pat' <- typePat pat tyT1'
    let gamma' = foldl (\ g (b, t) -> HM.insert b t g) gamma
                 $ map (second fromResType)
                 $ WT.patTyBnds pat'
    (gamma'', e2', tyT2) <- typeSystem delta gamma' e2

    return (gamma'', WT.LetE pat' e1' e2', tyT2)

  {-
               Delta, Gamma |– fun: T1 -> T2 -> ... Tm -> Tf  n<=m
     Delta, Gamma |-t1: T1  Delta, Gamma |- t2:T2  ...  Delta, Gamma |- tn:Tn
  ===================================================================================
                  Delta, Gamma |– fun t1 t2 ... tn : Tf
  -}
  (FrLang.AppE fun args) ->
    let
      assocArgWithType [] (_:_) =
        throwError $ "Too many argumemts in function application: " <> show (FrLang.AppE fun args)
      assocArgWithType l [] = return ([], l)
      assocArgWithType (t:ts) (arg:args) = do
        (argsAndTy, pendingTy) <- assocArgWithType ts args
        return ((arg,t) : argsAndTy, pendingTy)
    in do
      (gamma', fun', funTy) <- typeSystem delta gamma fun

      resTy <- case funTy of
        Res.TypeFunction (Res.FunType ins out) -> do
            (_, pendingArgsTy) <- assocArgWithType (toList ins) $ toList args
            case pendingArgsTy of
              [] -> return out
              (x:xs)  -> return $ Res.TypeFunction (Res.FunType (x:|xs) out)
        Res.TypeFunction (Res.STFunType sin ins out) -> do
            (_, pendingArgsTy) <- assocArgWithType ins $ toList args
            return $ Res.TypeFunction (Res.STFunType sin pendingArgsTy out)
        t -> throwError $ "Type Error: First argument of function application is not a function, but has type: " <> show t

      (_, args', _argsTy) <- neUnzip3 <$> mapM (typeSystem delta gamma) args

      return (gamma', WT.AppE fun' args', resTy)

  {-
              Delta, Gamma, p1:T1, p2:T2, ..., pn:Tn |- e:Te
  =================================================================
      Delta, Gamma |- Lambda p1 p2 .. pn. e : T1 -> T2 -> ... -> Tn -> Te
  -}
  (FrLang.LamE pats expr) ->
    let
      invariantGetGamma :: Binding -> StateT (Gamma Res.VarType ty) m (Res.VarType ty)
      invariantGetGamma bnd = do
        gam <- get
        case HM.lookup bnd gam of
          Nothing -> throwError $ "Invariant broken: pattern deleted while typing."
          Just ty' -> (modify $ HM.delete bnd) >> return ty'

      typePatFromGamma :: FrLang.Pat ty -> StateT (Gamma Res.VarType ty) m (WT.Pat ty)
      typePatFromGamma (FrLang.VarP bnd t) = (\ t' -> WT.VarP bnd <$> maxType t t') =<< invariantGetGamma bnd
      typePatFromGamma (FrLang.TupP ps) = WT.TupP <$> mapM typePatFromGamma ps
    in do
      let bndsAndTypes = map FrLang.patTyBnds pats
      let gamma' = foldl (\ g (b, t) -> HM.insert b t g) gamma $ neConcat bndsAndTypes
      (gamma'', expr', tyE) <- typeSystem delta gamma' expr
      (pats', gamma''') <- runStateT (mapM typePatFromGamma pats) gamma''
      let ty = Res.TypeFunction $ Res.FunType (map WT.patType pats') tyE

      return (gamma''', WT.LamE pats' expr', ty)
  {-
      Delta, Gamma |- state : S     Delta, Gamma |- method : S -> Tm
  ========================================================================
                Delta, Gamma |- Bind state method : Tm
  -}
  (FrLang.BindE state method) -> do
    (gamma',  state' , stateTy ) <- typeSystem delta gamma state
    (gamma'', method', methodTy) <- typeSystem delta gamma method
    ty <- case methodTy of
            Res.TypeFunction (Res.STFunType sTy _ resTy) | sTy == stateTy -> return resTy
            _ -> throwError $ "Typing error. State types do not match: " <> show (FrLang.BindE state method)

    return (gamma'', WT.BindE state' method', ty)

  {-
      Delta, Gamma |- cond : Bool    Delta, Gamma |- tTrue : T   Delta, Gamma |- tFalse : T
  ===========================================================================================
                  Delta, Gamma |- If cond tTrue tFalse : T
  -}
  (FrLang.IfE cond tTrue tFalse) -> do
    (_, cond', condTy) <- typeSystem delta gamma cond
    if condTy == Res.TypeBool
    then return ()
    else throwError "Type error: condition input does not have type bool!"

    (_, tTrue', tTrueTy) <- typeSystem delta gamma tTrue
    (gamma', tFalse', tFalseTy) <- typeSystem delta gamma tFalse
    if tTrueTy == tFalseTy
    then return ()
    else throwError "Type error: conditional branches have different types."

    return (gamma', WT.IfE cond' tTrue' tFalse', tTrueTy)

  {-
   Delta, Gamma |- cond : Bool       Delta, Gamma |- body : T
  ===============================================================
            Delta, Gamma |- While cond body : Unit

  -}
  (FrLang.WhileE cond body) -> do
    (_, cond', condTy) <- typeSystem delta gamma cond
    if condTy == Res.TypeBool
    then return ()
    else throwError "Type error: condition input for while loop does not have type bool!"

    (gamma', body', _bodyTy) <- typeSystem delta gamma body

    return (gamma', WT.WhileE cond' body', Res.TypeUnit)

  {-
      Delta, Gamma |- generator : T1<T2>     Delta, Gamma, x:T2 |- loopFun : T3
  ===============================================================================
          Delta, Gamma |- MapE loopFun generator : T1<T3>
  -}
  (FrLang.MapE loopFun gen) ->  do
    (_, gen', genTy) <- typeSystem delta gamma gen
    elemTy <- case genTy of
                Res.TypeList eTy -> return eTy
                _ -> throwError "Type error: loop generator is not a list!"

    (gamma', loopFun', loopFunTy) <- typeSystem delta gamma loopFun

    return (gamma', WT.MapE loopFun' gen', Res.TypeList loopFunTy)

  {-
      Delta, Gamma |- e1:T1    Delta, Gamma |- cont : T2
  =======================================================
          Delta, Gamma |- Stmt e1 cont : T2
  -}
  (FrLang.StmtE e1 cont) -> do
    (_, e1', _e1Ty) <- typeSystem delta gamma e1
    (gamma', cont', contTy) <- typeSystem delta gamma cont
    return (gamma', WT.StmtE e1' cont', contTy)

  {-
      Delta, Gamma |- e1:T1  Delta, Gamma |- e2:T2   ...   Delta, Gamma |- en: Tn
  ======================================================================================
        Delta, Gamma |- TupE [e1, e2 ... , en] : TupleTy [T1, T2, .. , Tn]
  -}
  (FrLang.TupE exprs) -> do
    (gamma' :| _, exprs',exprsTy ) <- neUnzip3 <$> mapM (typeSystem delta gamma) exprs
    return (gamma', WT.TupE exprs', Res.TupleTy exprsTy)

  {-
       x:T in Gamma
  ========================
    Delta, Gamma |– x: T
  -}
  (FrLang.VarE bnd ty) -> handleVar gamma bnd ty
  (FrLang.LitE (EnvRefLit bnd ty)) -> (\(g, _, ty') -> (g, WT.LitE $ Res.EnvRefLit bnd ty', ty')) <$> handleVar gamma bnd ty

  {-
     l:T in Delta
  ========================
    Delta, Gamma |- l : T
  -}
  (FrLang.LitE (FunRefLit (FunRef qbnd id ty))) -> do
      case HM.lookup qbnd delta of
        Just ty' ->
          ((\ty'' ->
            (HM.empty, WT.LitE $ Res.FunRefLit $ Res.FunRef qbnd id ty', ty'')) .
          Res.TypeFunction) <$> maxFunType ty ty'
        Nothing -> throwError $ "Type error: missing type information for function literal:" <> show qbnd

  {-
  ==================
    Gamma |- l : HostType
  -}
  (FrLang.LitE (NumericLit n)) -> return (HM.empty, WT.LitE $ Res.NumericLit n, Res.TypeNat) -- FIXME incorrect. we should not have this in this language!
  (FrLang.LitE (BoolLit b))    -> return (HM.empty, WT.LitE $ Res.BoolLit b   , Res.TypeBool)
  (FrLang.LitE UnitLit)        -> return (HM.empty, WT.LitE Res.UnitLit       , Res.TypeUnit)
  (FrLang.LitE (StringLit s))  -> return (HM.empty, WT.LitE $ Res.StringLit s , Res.TypeString)
  where
    resolveBothDirections bnd ty1 ty2 =
      case toResType ty1 of
        Just ty1' -> maxType ty2 ty1'
        Nothing ->
          case toResType ty2 of
            Just ty2' -> maxType ty1 ty2'
            Nothing -> throwError $ "Invariant broken: var in context has no type: " <> show bnd

    handleVar gamma bnd ty = do
      case HM.lookup bnd gamma of
        Just ty1 -> (\ty' -> (HM.singleton bnd ty', WT.VarE bnd ty', ty')) <$> resolveBothDirections bnd ty ty1
        Nothing ->
          let qbnd = QualifiedBinding (makeThrow []) bnd
          in case HM.lookup qbnd delta of
            Just ty1 ->
              (\ty' ->
                (HM.empty, WT.LitE $ Res.FunRefLit $ Res.FunRef qbnd Nothing ty1, ty'))
              <$> maxType ty (Res.TypeFunction ty1)
            Nothing -> throwError "Invariant broken: var not in typing context(s)."

typePat :: ErrAndLogM m => FrLang.Pat ty -> Res.VarType ty -> m (WT.Pat ty)
typePat pat newTy = do
  let oldTy = FrLang.patType pat
  newTy' <- maxType oldTy newTy
  go pat newTy'
  where
    go (FrLang.VarP bnd _) t = return $ WT.VarP bnd t
    go (FrLang.TupP (p:|ps)) (Res.TupleTy (pTy:|psTy)) = (\x xs -> WT.TupP $ x:|xs) <$> go p pTy <*> (mapM (uncurry go) $ zip ps psTy)
    go (FrLang.TupP _) _ = throwError "The impossible happened"

maxType :: ErrAndLogM m => VarType ty -> Res.VarType ty -> m (Res.VarType ty)
-- ^ equal types
maxType TypeNat Res.TypeNat = return Res.TypeNat
maxType TypeBool Res.TypeBool = return Res.TypeBool
maxType TypeUnit Res.TypeUnit = return Res.TypeUnit
maxType TypeString Res.TypeString = return Res.TypeString
maxType (TypeList x) (Res.TypeList y) = Res.TypeList <$> maxType x y
maxType (Type t1) (Res.Type t2) | t1 == t2 = return $ Res.Type t2
-- ^ unequal host types -> for know thats an error, but actually we need to resort to Rust here e.g Self vs ActualType => ActuaType
maxType (Type t1) (Res.Type t2) = throwError $ "Type error. Comparing types " <> show t1 <> " and " <> show t2
maxType (TupleTy (x:|xs)) (Res.TupleTy (y:|ys)) =
  if length xs == length ys
  then do
    xs' <- mapM (uncurry maxType) $ zip xs ys
    x' <- maxType x y
    return $ Res.TupleTy (x':|xs')
  else throwError "Type error: list with different length detected."
maxType (TypeFunction f) (Res.TypeFunction g) = Res.TypeFunction <$> maxFunType f g
maxType TypeVar t2 = return t2

maxFunType :: ErrAndLogM m => FunType ty -> Res.FunType ty -> m (Res.FunType ty)
maxFunType (FunType ins out) (Res.FunType rins rout) =
  Res.FunType <$> mapM (uncurry maxType) (NE.zip ins rins) <*> maxType out rout
maxFunType (STFunType sin ins out) (Res.STFunType rsin rins rout) =
  Res.STFunType <$> maxType sin rsin <*> mapM (uncurry maxType) (zip ins rins) <*> maxType out rout

