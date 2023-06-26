{-# LANGUAGE LambdaCase #-}

module Ohua.Frontend.TypeSystem where


import qualified Data.HashMap.Lazy as HM

import Ohua.UResPrelude hiding (getVarType)
import qualified Ohua.Prelude as Res
  ( Lit(..), VarType(..), FunType(..))

import Ohua.Frontend.Types
import Ohua.Frontend.PPrint ()
import qualified Ohua.Frontend.Lang as FrLang
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
import qualified Ohua.Frontend.WellTyped as WT
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

--import Ohua.Integration.Rust.TypeHandling
import Ohua.Frontend.TypePropagation

import Control.Exception (assert, throw)

-- | We go through expressions bottom-up and collect types of variables. Bottom-up implies, that we encounter the usage of a variable, bevor it's assignment.
--   Therefor we can use function argument types, extracted in the step before to annotate the argument variables at their usage site and via the context also in
--   the expression in wich they are bound. There can be variables e.g. the return variable of a function, that are not used as an argument. We can type them also,
--   by tracing the current 'innermost' return type.
-- ToDo: Monadify
toWellTyped :: ErrAndLogM m => FrLang.Expr ty -> m (WT.Expr ty)
toWellTyped expr =  typeSystem HM.empty

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

-- | We pass in the return type of the algorithm and the algorithm and recursively whether each expression has the type it's
--   supposed to have such that expected return type and calculated type match, or (because we have Unknown types) that one just more specific than the other
typeSystem :: ErrAndLogM m => Gamma VarType ty, FrLang.Expr ty -> m (Gamma Res.VarType ty, WT.Expr ty, Res.VarType ty)
typeSystem gamma = \case
    (FrLang.LetE (FrLang.VarP bnd tyT1) e1 e2) -> do
    {-
      Delta, Gamma |– e1: T1     Delta, Gamma, x:(max X T1) |– e2: T2
    ==================================================================
               Delta, Gamma |– let (x:X) e1 e2 : T2
    -}
        (_, e1', tyT1') <- typeSystem gamma e1
        let tyT1''' = maxType tyT1 tyT1'
        (gamma', e2', tyT2) <- typeSystem (HM.insert bnd tyT1'' gamma) e2

        return (gamma', WT.LetE (WT.VarP bnd tyT1'') e1'' e2', tyT2')

    (FrLang.AppE fun args) ->
    {-
     Delta, Gamma |– fun: T1 -> T2 -> ... Tm -> Tf  n<=m  Delta, Gamma |-t1: T1     Delta, Gamma |- t2:T2 ... Delta, Gamma |- tn:Tn
    ============================================================================================================================
                              Delta, Gamma |– fun t1 t2 ... tn : Tf
    -}
      let
        handleFun [] (_:_) = throwError "Too many argumemts in function application."
        handleFun l [] = l
        handleFun (x:xs) (arg:args) = do
          (argsAndTy, pendingTy) <- handleFun xs args
          return ((arg,x) : argsAndTy, pendingTy)
      in do
        (gamma', fun', funTy) <- typeSystem gamma fun

        (gamma'', resTy) <- case funTy of
          Res.FunctionType (Res.FunType ins out) -> do
            (argsAndTy, pendingTy) <- handFun ins args
            foldM (\g (b, t) ->
                     case b of
                       Var bnd ty -> case HM.lookup bnd gamma of
                                       Nothing -> throwError "Invariant broken: var not in context"
                                       Just ty' -> HM.insert bnd (fromResType $ maxType ty' ty) gamma
                       _ -> g
                     HM.insert ) gamma argsAndTy
          Res.FunctionType (Res.STFunType sin ins out) -> do
            handleFun ins args
          Nothing -> throwError "First argument of function application is not a function!"

        (_, args', _argsTy) <- unzip3 $ mapM (typeSystem gamma'') args

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
        (gamma'', expr', tyE) <- typeSystem gamma' expr
        argTypes <-
              map (\b -> case HM.lookup b gamma'' of
                           Nothing -> throwError $ "Invariant broken: pattern deleted while typing."
                           Just t -> t
                  )
              $ map fst bndsAndTypes
        let ty = TypeFunction $ FunType argTypes tyE

        {-
        body_ctxt <- get
        -- ToDo; Check if tryUpdate needs to update the context,i.e. if there's any use of that. If not, we can save us the deletion step for the patterns afterwars.
        let pats' = map (tryUpdate body_ctxt) pats
        -- Remove the local vars from the context
        let bound_names = map fst bndsAndTypes
        let outer_context = foldr HM.delete body_ctxt bound_names
        put outer_context
        traceM $ "Returning LamE " <> show pats' <> show expr'
        traceM $ "In context " <> show outer_context
        -}

        return (gamma, WT.LamE pats' expr', ty)

    (BindE state method) -> do
    {-
        Delta, Gamma |- state : S      Delta, Gamma |- method : S -> Tm
    ========================================================================
                  Delta, Gamma |- Bind state method : Tm
    -}
        (gamma',  state' , stateTy ) <- typeSystem gamma state
        (gamma'', method', methodTy) <- typeSystem gamma method
        ty <- case methodTy of
                Res.TypeFunction (Res.STFunType sTy _ _ resTy) | sTy = stateTy -> return resTy
                _ -> throwError "Typing error in BindE."

        return (gamma'', WT.BindE state' method', ty)

    (IfE cond tTrue tFalse) -> do
    {-
        Delta, Gamma |- cond : Bool    Delta, Gamma |- tTrue : T   Delta, Gamma |- tFalse : T
    ===========================================================================================
                    Delta, Gamma |- If cond tTrue tFalse : T
    -}
        (_, cond', condTy) <- typeSystem gamma cond
        if condTy == Res.TypeBool
        then return ()
        else throwError "Type error: condition input does not have type bool!"

        (_, tTrue', tTrueTy) <- typeSystem gamma tTrue
        (_, tFalse', tFalseTy) <- typeSystem gamma tFalse
        if tTrueTy == tFalseTy
        then return ()
        else throwError "Type error: conditional branches have different types."

        return (gamma', WT.IfE cond' tTrue' tFalse', ty)

    (WhileE cond body) -> do
    {-
     Delta, Gamma |- cond : Bool       Delta, Gamma |- body : T
    ===============================================================
              Delta, Gamma |- While cond body : Unit

    -}
        (_, cond', condTy) <- typeSystem gamma cond
        if condTy == Res.TypeBool
        then return ()
        else throwError "Type error: condition input for while loop does not have type bool!"

        (gamma', cond', _bodyTy) <- typeSystem gamma body

        return (gamma', WT.WhileE cond' body', Res.TypeUnit)

    (MapE loopFun gen) ->  do
    {-
        Delta, Gamma |- generator : T1<T2>     Delta, Gamma, x:T2 |- loopFun : T3
    ===============================================================================
            Delta, Gamma |- MapE loopFun generator : T1<T3>
    -}
        (_, gen', genTy) <- typeSystem gamma gen
        elemTy <- case genTy of
            Res.TypeList eTy -> eTy
            _ -> throwError "Type error: loop generator is not a list!"

        (gamma', loopFun', loopFunTy) <- typeSystem gamma loopFun

        return (gamma', WT.MapE loopFun' gen', Res.TypeList loopFunTy)

    (StmtE e1 cont) -> do
    {-
        Delta, Gamma |- e1:T1    Delta, Gamma |- cont : T2
    =======================================================
            Delta, Gamma |- Stmt e1 cont : T2
    -}
        (_, e1', _e1Ty) <- typeSystem gamma e1
        (gamma', cont', contTy) <- typeSystem gamma cont
        return (gamma', WT.StmtE e1' cont', contTy)

--    (SeqE e1 cont) -> do
--
--    {-
--    Actually that expression is only introduced after this point and should be eliminated anyways :-/. So we have it for completeness but do not need to handle it actually
--
--        Delta, Gamma |- cont : T   Delta, Gamma |- e1 : T1
--    ========================================================
--            Delta, Gamma |- Seq e1 cont : T
--
--    -}
--        -- same as for Stmt, we have no expectations for e1
--        e1' <- typeSystem typeUnknown e1
--        cont' <- typeSystem ty cont
--        return $ SeqE e1' cont'

    (TupE exprs) -> do
    {-
        Delta, Gamma |- e1:T1  Delta, Gamma |- e2:T2   ...   Delta, Gamma |- en: Tn
    ======================================================================================
          Delta, Gamma |- TupE [e1, e2 ... , en] : TupleTy [T1, T2, .. , Tn]

    -}
        (gamma' :| _, exprs',exprsTy ) <- unzip3 <$> mapM (typeSystem gamma) exprs
        return (gamma', WT.TupE exprs', Res.TupleTy exprsTy)

    v@(VarE bnd vty) -> do
    {-
         x:T in Gamma
    ========================
      Delta, Gamma |– x: T
    -}
        ty <- case HM.lookup bnd ctxt of
                   Nothing -> throwError "Invariant broken: var not in typing context."
                   Just t -> return t

        ty' <- case toResType ty of
                   Just t -> maxType vty t
                   Nothing -> case toResType vty of
                                  Just t' -> maxType ty t'
                                  Nothing -> throwError $ "Invariant broken: var in context has no type: " <> show bnd

        return (HM.singleton bnd ty', WT.VarE, ty')

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
maxType (TupleTy xs) (Res.TupleTy ys) =
  if length xs == length ys
  then Res.TupleTy <$> mapM (maxType . uncurry) $ zip xs ys
  else throwError "Type error: list with different length detected."
maxType (TypeFunction f) (Res.TypeFunction g) = Res.TypeFunction <$> maxFunType f g
maxType TypeVar t2 = return t2
maxType t1 t2 | t1 /= t2 = error $ "Typing error. Comparing types " <> show t1 <> " and " <> show t2
{-
-- Problem: For an arbitrary host ty, we can not tell if it should be coercible to internal Types we assign to literals e.g. NumericLit/Boollit etc.
-- What we need are HostLits, with a String representation and a Host Type. So far we don't have that andI'll hack arround the type system by always giving
-- precedence to the host type.
maxType t@(Type (HostType ty)) someInternalType = t
maxType someInternalType t@(Type (HostType ty)) = t
-}

maxFunType :: ErrAndLogM m => FunType ty -> Res.FunType ty -> m (Res.FunType ty)
maxFunType (FunType ins out) (Res.FunType rins rout) = Res.FunType <$> mapM maxType ins rins <*> maxType out rout
maxFunType (STFunType sin ins out) (Res.STFunType rsin rins rout) =
  Res.STFunType <$> maxType sin rsin <*> mapM maxType ins rins <*> maxType out rout

toResType :: VarType ty -> Maybe (Res.VarType ty)
toResType TypeNat = Just Res.TypeNat
toResType TypeBool = Just Res.TypeBool
toResType TypeUnit = Just Res.TypeUnit
toResType TypeString = Just Res.TypeString
toResType (TypeList l) = Res.TypeList <$> toResType l
toResType (Type t) = Just $ Res.Type t
toResType (TupleTy xs) = Res.TupleTy <$> sequence $ map toResType l
toResType (TypeFunction f) = Res.TypeFunction <$> toResFunType f
toResType TypeVar = Nothing

toResFunType :: FunType ty -> Maybe (Res.FunType ty)
toResFunType (FunType ins out) = Res.FunType <$> (sequence $ map toResType ins) <*> toResType out
toResFunType (STFunType sin ins out) =
  Res.STFunType <$>
  toResType ins <*>
  (sequence $ map toResType ins) <*>
  toResType out

fromResType :: Res.VarType ty -> VarType ty
fromResType Res.TypeNat = TypeNat
fromResType Res.TypeBool = Just Res.TypeBool
fromResType Res.TypeUnit = Just Res.TypeUnit
fromResType Res.TypeString = TypeString
fromResType (Res.TypeList l) = TypeList $ fromResType l
fromResType (Res.Type t) = Type t
fromResType (Res.TupleTy xs) = TupleTy $ map fromResType l
fromResType (Res.TypeFunction f) = TypeFunction $ fromResFunType f

fromResFunType :: Res.FunType ty -> FunType ty
fromResFunType (Res.FunType ins out) = FunType (map toResType ins) $ toResType out
fromResFunType (Res.STFunType sin ins out) =
  STFunType
  (fromResType ins)
  (map fromResType ins)
  (fromResType out)
