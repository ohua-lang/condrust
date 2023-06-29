{-# LANGUAGE ScopedTypeVariables #-}

module Ohua.Frontend.TypeSystem
  (Delta(..), toWellTyped)
where


import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE

import Ohua.UResPrelude hiding (getVarType)
import qualified Ohua.Prelude as Res
  ( Lit(..), VarType(..), FunType(..))
import Ohua.Types.Casts

import Ohua.Frontend.PPrint ()
import qualified Ohua.Frontend.Lang as FrLang
    ( Expr(..)
    , Pat(TupP, VarP)
    , patTyBnds
    , patType
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
toWellTyped delta e = do
  (_gamma, e, _ty) <- typeSystem delta HM.empty e
  return e

typeSystem :: forall ty m
           .  ErrAndLogM m
           => Delta ty
           -> Gamma VarType ty
           -> FrLang.Expr ty
           -> m (Gamma Res.VarType ty, WT.Expr ty, Res.VarType ty)
typeSystem delta gamma = \case
    (FrLang.LetE pat e1 e2) -> do
    {-
      Delta, Gamma |– e1: T1     Delta, Gamma, x:(max X T1) |– e2: T2
    ==================================================================
               Delta, Gamma |– let (x:X) e1 e2 : T2
    -}
        (_, e1', tyT1') <- typeSystem delta gamma e1
        pat' <- typePat pat tyT1'
        let gamma' =
              foldl (\ g (b, t) -> HM.insert b t g) gamma
              $ map (second fromResType)
              $ WT.patTyBnds pat'
        (gamma', e2', tyT2) <- typeSystem delta gamma' e2

        return (gamma', WT.LetE pat' e1' e2', tyT2)

    (FrLang.AppE fun args) ->
    {-
                 Delta, Gamma |– fun: T1 -> T2 -> ... Tm -> Tf  n<=m
       Delta, Gamma |-t1: T1  Delta, Gamma |- t2:T2  ...  Delta, Gamma |- tn:Tn
    ===================================================================================
                    Delta, Gamma |– fun t1 t2 ... tn : Tf
    -}
      let
        assocArgWithType [] (_:_) =
          throwError $ "Too many argumemts in function application: " <> show (FrLang.AppE fun args)
        assocArgWithType l [] = return ([], l)
        assocArgWithType (x:xs) (arg:args) = do
            (argsAndTy, pendingTy) <- assocArgWithType xs args
            return ((arg,x) : argsAndTy, pendingTy)
        handleFun ins args = do
            (argsAndTy, pendingArgsTy) <- assocArgWithType ins args
            gamma' <-
                foldM (\g (b, t) ->
                         case b of
                             FrLang.VarE bnd ty ->
                                 case HM.lookup bnd g of
                                     Nothing -> throwError "Invariant broken: var not in context"
                                     Just ty' -> do
                                         ty'' <- case toResType ty of
                                                     Nothing -> return ty'
                                                     Just ty'' -> fromResType <$> maxType ty' ty''
                                         return $ HM.insert bnd ty'' g
                             _ -> return g)
                      gamma
                      argsAndTy
            return (gamma', pendingArgsTy)
      in do
        (gamma', fun', funTy) <- typeSystem delta gamma fun

        (gamma'', resTy) <- case funTy of
          Res.TypeFunction (Res.FunType ins out) -> do
              (gamma'', pendingArgsTy) <- handleFun (toList ins) $ toList args
              case pendingArgsTy of
                [] -> return (gamma'', out)
                (x:xs)  -> return (gamma'', Res.TypeFunction (Res.FunType (x:|xs) out))
          Res.TypeFunction (Res.STFunType sin ins out) -> do
              (gamma'', pendingArgsTy) <- handleFun ins $ toList args
              return (gamma'', Res.TypeFunction (Res.STFunType sin pendingArgsTy out))
          t -> throwError $ "Type Error: First argument of function application is not a function, but has type: " <> show t

        (_, args', _argsTy) <- neUnzip3 <$> mapM (typeSystem delta gamma'') args

        return (gamma', WT.AppE fun' args', resTy)
    (FrLang.LamE pats expr) ->
    {-
                Delta, Gamma, p1:T1, p2:T2, ..., pn:Tn |- e:Te
    =================================================================
        Delta, Gamma |- Lambda p1 p2 .. pn. e : T1 -> T2 -> ... -> Tn -> Te
    -}
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

    (FrLang.BindE state method) -> do
    {-
        Delta, Gamma |- state : S     Delta, Gamma |- method : S -> Tm
    ========================================================================
                  Delta, Gamma |- Bind state method : Tm
    -}
        (gamma',  state' , stateTy ) <- typeSystem delta gamma state
        (gamma'', method', methodTy) <- typeSystem delta gamma method
        ty <- case methodTy of
                Res.TypeFunction (Res.STFunType sTy _ resTy) | sTy == stateTy -> return resTy
                _ -> throwError $ "Typing error. State types do not match: " <> show (FrLang.BindE state method)

        return (gamma'', WT.BindE state' method', ty)

    (FrLang.IfE cond tTrue tFalse) -> do
    {-
        Delta, Gamma |- cond : Bool    Delta, Gamma |- tTrue : T   Delta, Gamma |- tFalse : T
    ===========================================================================================
                    Delta, Gamma |- If cond tTrue tFalse : T
    -}
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

    (FrLang.WhileE cond body) -> do
    {-
     Delta, Gamma |- cond : Bool       Delta, Gamma |- body : T
    ===============================================================
              Delta, Gamma |- While cond body : Unit

    -}
        (_, cond', condTy) <- typeSystem delta gamma cond
        if condTy == Res.TypeBool
        then return ()
        else throwError "Type error: condition input for while loop does not have type bool!"

        (gamma', body', _bodyTy) <- typeSystem delta gamma body

        return (gamma', WT.WhileE cond' body', Res.TypeUnit)

    (FrLang.MapE loopFun gen) ->  do
    {-
        Delta, Gamma |- generator : T1<T2>     Delta, Gamma, x:T2 |- loopFun : T3
    ===============================================================================
            Delta, Gamma |- MapE loopFun generator : T1<T3>
    -}
        (_, gen', genTy) <- typeSystem delta gamma gen
        elemTy <- case genTy of
            Res.TypeList eTy -> return eTy
            _ -> throwError "Type error: loop generator is not a list!"

        (gamma', loopFun', loopFunTy) <- typeSystem delta gamma loopFun

        return (gamma', WT.MapE loopFun' gen', Res.TypeList loopFunTy)

    (FrLang.StmtE e1 cont) -> do
    {-
        Delta, Gamma |- e1:T1    Delta, Gamma |- cont : T2
    =======================================================
            Delta, Gamma |- Stmt e1 cont : T2
    -}
        (_, e1', _e1Ty) <- typeSystem delta gamma e1
        (gamma', cont', contTy) <- typeSystem delta gamma cont
        return (gamma', WT.StmtE e1' cont', contTy)

    (FrLang.TupE exprs) -> do
    {-
        Delta, Gamma |- e1:T1  Delta, Gamma |- e2:T2   ...   Delta, Gamma |- en: Tn
    ======================================================================================
          Delta, Gamma |- TupE [e1, e2 ... , en] : TupleTy [T1, T2, .. , Tn]

    -}
        (gamma' :| _, exprs',exprsTy ) <- neUnzip3 <$> mapM (typeSystem delta gamma) exprs
        return (gamma', WT.TupE exprs', Res.TupleTy exprsTy)

    v@(FrLang.VarE bnd vty) -> do
    {-
         x:T in Gamma
    ========================
      Delta, Gamma |– x: T
    -}
        (gamma', ty) <- handleVar gamma bnd vty
        return (gamma', WT.VarE bnd ty, ty)

    (FrLang.LitE (EnvRefLit bnd vty)) -> do
        (gamma', ty) <- handleVar gamma bnd vty
        return (gamma', WT.LitE $ Res.EnvRefLit bnd ty, ty)

    (FrLang.LitE (FunRefLit f)) -> do
    {-
       l:T in Delta
    ========================
      Delta, Gamma |- l : T
    -}
        -- FIXME this case seems wrong to me unless we encode TypeVar as Type -> TypeVar!
        --       there must be a step in the frontend that does this. Where is it?!
        throwError "TODO"

    (FrLang.LitE l) -> do
    {-
    ==================
      Gamma |- l : HostType
    -}
        throwError "TODO"

    where
        handleVar gamma bnd vty = do
            ty <- case HM.lookup bnd gamma of
                      Nothing -> throwError "Invariant broken: var not in typing context."
                      Just t -> return t

            ty' <- case toResType ty of
                       Just t -> maxType vty t
                       Nothing -> case toResType vty of
                                      Just t' -> maxType ty t'
                                      Nothing -> throwError $ "Invariant broken: var in context has no type: " <> show bnd

            return (HM.singleton bnd ty', ty')

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
{-
-- Problem: For an arbitrary host ty, we can not tell if it should be coercible to internal Types we assign to literals e.g. NumericLit/Boollit etc.
-- What we need are HostLits, with a String representation and a Host Type. So far we don't have that andI'll hack arround the type system by always giving
-- precedence to the host type.
maxType t@(Type (HostType ty)) someInternalType = t
maxType someInternalType t@(Type (HostType ty)) = t
-}

maxFunType :: ErrAndLogM m => FunType ty -> Res.FunType ty -> m (Res.FunType ty)
maxFunType (FunType ins out) (Res.FunType rins rout) =
  Res.FunType <$> mapM (uncurry maxType) (NE.zip ins rins) <*> maxType out rout
maxFunType (STFunType sin ins out) (Res.STFunType rsin rins rout) =
  Res.STFunType <$> maxType sin rsin <*> mapM (uncurry maxType) (zip ins rins) <*> maxType out rout

