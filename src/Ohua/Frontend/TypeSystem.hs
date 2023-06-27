{-# LANGUAGE LambdaCase #-}

module Ohua.Frontend.TypeSystem
  ( RawExpr(..)
  , toWellTyped
  )
where


import qualified Data.HashMap.Lazy as HM

import Ohua.UResPrelude hiding (getVarType)
import qualified Ohua.Prelude as Res
  ( Lit(..), VarType(..), FunType(..))

--import Ohua.Frontend.Types
import Ohua.Frontend.PPrint ()
import qualified Ohua.Frontend.Lang as FrLang
    ( Expr(..)
    , Pat(TupP, VarP, WildP)
    )
import qualified Ohua.Frontend.WellTyped as WT
    ( Expr(..)
    ,  Pat(TupP, VarP, WildP)
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

toWellTyped :: ErrAndLogM m => Delta ty -> FrLang.Expr ty -> m (WT.Expr ty)
toWellTyped delta = do
  (_gamma, e, _ty) <- typeSystem delta HM.empty
  return e

typeSystem :: ErrAndLogM m
           => Delta ty
           -> Gamma VarType ty
           -> FrLang.Expr ty
           -> m (Gamma Res.VarType ty, WT.Expr ty, Res.VarType ty)
typeSystem delta gamma = \case
    (FrLang.LetE (FrLang.VarP bnd tyT1) e1 e2) -> do
    {-
      Delta, Gamma |– e1: T1     Delta, Gamma, x:(max X T1) |– e2: T2
    ==================================================================
               Delta, Gamma |– let (x:X) e1 e2 : T2
    -}
        (_, e1', tyT1') <- typeSystem delta gamma e1
        let tyT1'' = maxType tyT1 tyT1'
        (gamma', e2', tyT2) <- typeSystem delta (HM.insert bnd tyT1'' gamma) e2

        return (gamma', WT.LetE (WT.VarP bnd tyT1'') e1' e2', tyT2)

    (FrLang.AppE fun args) ->
    {-
                 Delta, Gamma |– fun: T1 -> T2 -> ... Tm -> Tf  n<=m
       Delta, Gamma |-t1: T1     Delta, Gamma |- t2:T2 ... Delta, Gamma |- tn:Tn
    ===================================================================================
                    Delta, Gamma |– fun t1 t2 ... tn : Tf
    -}
      let
        assocArgWithType [] (_:_) =
          throwError $ "Too many argumemts in function application: " <> show (FrLang.AppE fun args)
        assocArgWithType l [] = l
        assocArgWithType (x:xs) (arg:args) = do
            (argsAndTy, pendingTy) <- assocArgWithType xs args
            return ((arg,x) : argsAndTy, pendingTy)
        handleFun ins args = do
            (argsAndTy, pendingArgsTy) <- assocArgWithType ins args
            gamma' <-
              foldM (\g (b, t) ->
                       case b of
                         FrLang.VarE bnd ty ->
                           case HM.lookup bnd gamma of
                             Nothing -> throwError "Invariant broken: var not in context"
                             Just ty' -> HM.insert bnd (fromResType $ maxType ty' ty) gamma
                         _ -> g)
                    gamma
                    argsAndTy
            return (gamma', pendingArgsTy)
      in do
        (gamma', fun', funTy) <- typeSystem delta gamma fun

        (gamma'', resTy) <- case funTy of
          Res.TypeFunction (Res.FunType ins out) -> do
              (gamma'', pendingArgsTy) <- handleFun ins args
              case pendingArgsTy of
                [] -> return (gamma'', out)
                _  -> return (gamma'', Res.TypeFunction (Res.FunType pendingArgsTy out))
          Res.TypeFunction (Res.STFunType sin ins out) -> do
              (gamma'', pendingArgsTy) <- handleFun ins args
              return (gamma'', Res.TypeFunction (Res.STFunType sin pendingArgsTy out))
          Nothing -> throwError "First argument of function application is not a function!"

        (_, args', _argsTy) <- unzip3 $ mapM (typeSystem delta gamma'') args

        return (gamma', WT.AppE fun' args', resTy)

    (FrLang.LamE pats expr) -> do
    {-
                Delta, Gamma, p1:T1, p2:T2, ..., pn:Tn |- e:Te
    =================================================================
        Delta, Gamma |- Lamda p1 p2 .. pn. e : T1 -> T2 -> ... -> Tn -> Te
    -}
        let bndsAndTypes = concatMap patTyBnds pats
        let gamma' = foldr (\ g (b, t) -> HM.insert b t g) gamma bndsAndTypes
        -- ToDo: Here ty must be the return type of the expected type
        (gamma'', expr', tyE) <- typeSystem delta gamma' expr
        argTypes <-
              map (\b -> case HM.lookup b gamma'' of
                           Nothing -> throwError $ "Invariant broken: pattern deleted while typing."
                           Just t -> t
                  )
              $ map fst bndsAndTypes
        let ty = Res.TypeFunction $ Res.FunType argTypes tyE

        return (gamma, WT.LamE pats' expr', ty)

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
                _ -> throwError "Typing error. State types do not match: " <> show (FrLang.BindE state method)

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
            Res.TypeList eTy -> eTy
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
        (gamma' :| _, exprs',exprsTy ) <- unzip3 <$> mapM (typeSystem delta gamma) exprs
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
  Res.FunType <$> mapM (uncurry maxType) (zip ins rins) <*> maxType out rout
maxFunType (STFunType sin ins out) (Res.STFunType rsin rins rout) =
  Res.STFunType <$> maxType sin rsin <*> mapM (uncurry maxType) (zip ins rins) <*> maxType out rout

toResType :: VarType ty -> Maybe (Res.VarType ty)
toResType TypeNat = Just Res.TypeNat
toResType TypeBool = Just Res.TypeBool
toResType TypeUnit = Just Res.TypeUnit
toResType TypeString = Just Res.TypeString
toResType (TypeList l) = Res.TypeList <$> toResType l
toResType (Type t) = Just $ Res.Type t
toResType (TupleTy xs) = Res.TupleTy <$> (sequence $ map toResType xs)
toResType (TypeFunction f) = Res.TypeFunction <$> toResFunType f
toResType TypeVar = Nothing

toResFunType :: FunType ty -> Maybe (Res.FunType ty)
toResFunType (FunType ins out) = Res.FunType <$> (sequence $ map toResType ins) <*> toResType out
toResFunType (STFunType sin ins out) =
  Res.STFunType <$>
  toResType sin <*>
  (sequence $ map toResType ins) <*>
  toResType out

fromResType :: Res.VarType ty -> VarType ty
fromResType Res.TypeNat = TypeNat
fromResType Res.TypeBool = TypeBool
fromResType Res.TypeUnit = TypeUnit
fromResType Res.TypeString = TypeString
fromResType (Res.TypeList l) = TypeList $ fromResType l
fromResType (Res.Type t) = Type t
fromResType (Res.TupleTy xs) = TupleTy $ map fromResType xs
fromResType (Res.TypeFunction f) = TypeFunction $ fromResFunType f

fromResFunType :: Res.FunType ty -> FunType ty
fromResFunType (Res.FunType ins out) = FunType (map fromResType ins) $ fromResType out
fromResFunType (Res.STFunType sin ins out) =
  STFunType
  (fromResType sin)
  (map fromResType ins)
  (fromResType out)
