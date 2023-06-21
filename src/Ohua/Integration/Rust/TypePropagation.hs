module Ohua.Integration.Rust.TypePropagation where

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


propagateType:: (ErrAndLogM m, TypeContextM m) => VarType RustVarType -> FrLang.Expr RustVarType -> m (FrLang.Expr RustVarType)
propagateType ty = \case 
        -- We should have a funtion type at this point
        --- let a: x->y = LamE pats body => pats:x body:y 
        e@LamE{} -> case ty of 
              TypeFunction fty -> propagateFunType fty e
              other -> throwError $ "Cannot type the lambda expression " <> show e <> " with non-function type " <> show ty
        -- ToDo: this is not correct, there might be nested function types.  
        --- let a: x = e => e:x 
        e -> FrLang.applyToFinal (propagateReturnType ty) e

propagateFunType::(ErrAndLogM m, TypeContextM m) => FunType RustVarType -> FrLang.Expr RustVarType -> m (FrLang.Expr RustVarType)
propagateFunType fty (LamE pats body) = do
    let argTypes = pureArgTypes fty 
    let returnType = getReturnType fty
    body' <- propagateType returnType body
    -- FIXME: This isn't correct still. We'd need to add the typed patterns to the context and use them when passing the body again.
    return $ LamE (zipWith setPatType argTypes pats) body'
propagateFunType _fty other           = throwError $ "Cannot apply a function type to expression " <> show other 

propagateReturnType :: (ErrAndLogM m, TypeContextM m) => VarType RustVarType -> FrLang.Expr RustVarType -> m (FrLang.Expr RustVarType)
propagateReturnType tyNew expr = do
    ctxt <- get
    case expr of
        (VarE bnd _t) -> do modify (HM.insert bnd tyNew); return (VarE bnd tyNew)
        LitE (EnvRefLit bnd _ty) -> do modify (HM.insert bnd tyNew); return $ LitE (EnvRefLit bnd tyNew)
        LitE (FunRefLit (FunRef q i fty)) -> return $ LitE (FunRefLit (FunRef q i  (setReturnType tyNew fty)))
        -- ToDo: If we had a HostLiteral (instead of NumericLit etc.) we'd need to type here as well 
        lit@(LitE l) -> return lit

        e@(TupE (ex:| exs)) -> case tyNew of 
            TupleTy (t:|tys) -> do
                    e' <- propagateReturnType t ex
                    exs' <- mapM (uncurry propagateReturnType) (zip tys exs)
                    return $ TupE (e':| exs')
            other -> throwError $ "Cannot type Tuple " <> show e <> " with non-tuple-type " <> show tyNew
        -- ToDo: implement type return type propagation for missing expressions.
        otherE -> throwError $ "Implementing type propagation for " <> show otherE <> " is required"

propagateArgTypes:: [VarType RustVarType] -> FrLang.Expr RustVarType -> FrLang.Expr RustVarType
propagateArgTypes tys fun  = case fun of
    VarE bnd (TypeFunction fty) ->       let newFty = setFunType tys (getReturnType fty) fty
                                         in  VarE bnd (TypeFunction newFty)
    LitE (FunRefLit (FunRef q i fty)) -> let newFty = setFunType tys (getReturnType fty) fty
                                         in  LitE (FunRefLit (FunRef q i newFty))
    -- ToDo: This doesn't propagate the pattern types into the body, but we likely allready processed the body so just adding the types to the context wont help here 
    -- > FIX by propagation types to body. 
    LamE pats body -> LamE (zipWith setPatType tys pats) body
    BindE state fun -> BindE state (propagateArgTypes tys fun)


tryUpdate::  VarTypeContext -> FrLang.Pat RustVarType -> FrLang.Pat RustVarType
-- We want to update the pattern bound on a LHS of a Let expression
-- The two possible sources of type information are a) usage sites, in which case the name of the binding should be in the context 
-- and b) the return type of the RHS in which case the currentReturn type should be avaiblable and not of type Unknown 
tryUpdate ctxt = \case
    (FrLang.VarP bnd (Type (HostType Unknown))) -> do
        case HM.lookup bnd ctxt of
            Just rty | not (isUnknown rty) -> VarP bnd rty
            _  -> error $ "Couldn't type pattern " <> show bnd
    tp@(FrLang.TupP pats) | (isNothing (realPatType tp)) -> do
            let mUsedTypes = mapM (>>= (`HM.lookup` ctxt)) (FrLang.patBnd tp)
            case  mUsedTypes of
                (Just [rty]) | not (isUnknown rty) -> error "We haven't implemented destruction of Rust tuple types as function return types. Please remind us to fix this." --return (VarP bnd rty)
                (Just tys@(t:ts)) | length tys == length pats -> setPatType (TupleTy (t:|ts)) tp
                _  -> error $ "Couldn't type tuple binding " <> show tp
    vTyped@FrLang.VarP{} -> vTyped
    wp@FrLang.WildP{} -> wp  -- There's no value in typing a wild pattern because we type right to left here and wp's aren't used downstream




realPatType:: FrLang.Pat RustVarType -> Maybe (VarType RustVarType)
realPatType = \case
      FrLang.VarP _bnd  ty -> if isUnknown ty then Nothing else Just ty
      FrLang.WildP ty -> if isUnknown ty then Nothing else Just ty
      tp@(FrLang.TupP pats) ->
            let TupleTy tys = FrLang.patType tp
            in if any isUnknown tys then Nothing else Just (TupleTy tys)


-- ToDo: This is essentially the same as propagateReturnType, except that we have a slightly different context and do not need to 
-- manipulate the context -> Can we merge the functions?
replaceType ::(ErrAndLogM m) => VarType RustVarType -> FrLang.Expr RustVarType -> m (FrLang.Expr RustVarType)
replaceType tyNew e = case e of
  VarE bnd _ty -> return $ VarE bnd tyNew
  LitE (EnvRefLit b _ty) -> return $  LitE (EnvRefLit b tyNew)
  LitE (FunRefLit (FunRef q i fty)) -> return $ LitE (FunRefLit (FunRef q i  (setReturnType tyNew fty)))
  lit@(LitE l) -> if getLitType l == tyNew
                    then throwError $ "Return type of function " <> show tyNew <> " doesn't match type "<> show (getLitType l) <> " of returned literal"
                    else return lit
                  -- ToDo: We can also get TupE here, which needs to be handled


-- Question: Can we have functions as arguments? 
fullyTyped :: FunType RustVarType -> Bool
fullyTyped (FunType args retTy) = all (\case  (Type (HostType Unknown)) -> False; TypeFunction fty -> fullyTyped fty; _ -> True) (retTy: args)
fullyTyped (STFunType sTy argTys retTy) = all (\case (Type (HostType Unknown)) -> False; TypeFunction fty -> fullyTyped fty; _ -> True) (sTy: retTy: argTys)


-- Stole this (kind of) from Core/Util because I don't wantto import Core here
assertE :: (IsString s, MonadError s m, HasCallStack, Monoid s) => Bool -> s -> m ()
assertE True  _ = return ()
assertE False msg = throwError $ "TypingError: " <> msg
{-# INLINE assertE #-}