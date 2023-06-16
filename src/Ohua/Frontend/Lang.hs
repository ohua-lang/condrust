{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Ohua.Frontend.Lang
    ( Pat(..)
    , Expr(..)
    , exprType
    , funType
    , patType
    , patBnd
    , patTyBnds
    , setPatType
    , setExprFunType
    , PatF(..)
    , ExprF(..)
    , patterns
    , applyToFinal
    ) where

import Ohua.Prelude


import Control.Lens (Traversal')
import Control.Lens.Plated (Plated, gplate, plate)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import qualified Data.List.NonEmpty as NE
import GHC.Exts

data Pat ty
    = VarP Binding (VarType ty)
    -- Actualy a tuple pattern starts making sense at two or more patterns
    -- but it should have at least one. So I changed this from [] to NonEmpty
    | TupP (NonEmpty (Pat ty))
    | WildP (VarType ty)
    deriving (Show, Eq, Generic)

patType:: Pat ty -> VarType ty
patType = \case
    VarP _ ty -> ty
    TupP ps -> TupleTy (map patType ps)
    WildP ty -> ty

patBnd:: Pat ty -> [Maybe Binding]
patBnd = \case
    VarP bnd ty -> Just bnd : []
    TupP ps -> concatMap patBnd ps
    WildP ty -> Nothing: []

-- | Helper function to update the binding context from typed patterns
patTyBnds :: Pat ty -> [(Binding, VarType ty)]
patTyBnds = \case
    VarP bnd ty -> [(bnd, ty)]
    TupP ps -> concatMap patTyBnds ps
    WildP ty -> []

setPatType :: VarType ty -> Pat ty -> Pat ty
setPatType nty = \case
    VarP bnd ty -> VarP bnd nty
    TupP ps -> case nty of
        TupleTy tys -> TupP $ NE.map (uncurry setPatType) (NE.zip tys ps)
    WildP ty -> WildP nty

data Expr ty
    -- REMINDER: We need to wrap the host type in an VarType here, because
    -- the compiler will introdude variables typed as internal bool/unit/int 
    -- ... that also have to be representable
    = VarE Binding (VarType ty)
    | LitE (Lit ty)
    | LetE (Pat ty)
           (Expr ty)
           (Expr ty)
    | AppE (Expr ty)
           [Expr ty]
    | LamE [Pat ty]
           (Expr ty) -- ^ An expression creating a function
    | IfE (Expr ty)
          (Expr ty)
          (Expr ty)
    | WhileE (Expr ty) (Expr ty)
    | MapE (Expr ty) -- ^ Map expression that 'maps' its first argument to its second :: map f xs.
           (Expr ty)

    | BindE (Expr ty)
            (Expr ty) -- ^ @BindE state function@ binds @state@ to be operated on by @function@
    | StmtE (Expr ty)
            (Expr ty) -- ^ An expression with the return value ignored
    | SeqE (Expr ty)
           (Expr ty)
    -- ToDo: make it NonEmpty (see type porblem below)
    | TupE (FunType ty) [Expr ty] -- ^ create a tuple value that can be destructured
    deriving (Show, Generic)


exprType:: Expr ty -> VarType ty
exprType (VarE _b ty) = ty
-- We aim for "return types" here so if the literal is a function, 
-- this will evaluate to the functions return type
exprType (LitE  lit) = getLitType lit
exprType (LetE _p _e cont) = exprType cont
exprType (AppE fun args) = exprType fun
-- Type of an abstraction (\x:T1. term:T2) as generics are a 'problem' of the host language
-- we do not handle any subtitution here and just take the return type of the term
exprType (LamE _p term) = exprType term
exprType (IfE c t1 t2) = exprType t1 -- we could also use t2 as they should be equal
exprType (WhileE c body) = TypeUnit -- This will change downstream
-- Question: How can we know this one ? 
-- The return of a map (t1->t2) (c t1) should be (c t2) but we cannot recombine host types 
exprType (MapE fun container) = TypeList TypeUnit
-- The (explicit) return type of a function, bound to a state is still the return type of the function
exprType (BindE _s f) = exprType f
-- Question: Correct?
exprType StmtE{} = TypeUnit
-- This just makes shure e1 is evaluated although its result is ignored
exprType (SeqE e1 e2) = exprType e2
exprType (TupE f (e:es)) = TupleTy (exprType e :| map exprType es)
-- ToDo: This is wrong in every case where Unit != None
exprType (TupE f []) = TypeUnit


funType :: Expr ty -> Maybe (FunType ty)
funType e = case e of
        (VarE _bnd (TypeFunction fTy))   -> Just fTy
        (LitE (FunRefLit fRef))          -> Just $ getRefType fRef
        (LamE pats body)                 -> Just $ FunType (map patType pats) (exprType body)
        (BindE _state method)            -> funType method
        -- Question: What's the type of BindState at this point?
        other                            -> Nothing


-- If expressions have two possible exits
-- Question how to make this more elegant. It's not a normal traversal as we don't want to recurse
-- into every nested expression?
applyToFinal :: (Expr ty -> Expr ty) -> Expr ty -> Expr ty
applyToFinal fun =  \case
    e@VarE{} -> fun e
    e@LitE{} -> fun e
    e@TupE{} -> fun e
    LetE p e1 e2 -> LetE p e1  $ applyToFinal fun e2
    AppE f args -> AppE (applyToFinal fun f) args
    LamE p e -> LamE p $ applyToFinal fun e
    IfE b e1 e2 -> IfE b (applyToFinal fun e1)  (applyToFinal fun e2)
    WhileE e1 e2 -> WhileE e1 $ applyToFinal fun e2
    MapE e1 e2 -> MapE (applyToFinal fun e1) e2 -- we map a function to a container so the final return is the return of the function
    BindE e1 e2 -> BindE e1 $ applyToFinal fun e2
    StmtE e1 e2 -> StmtE e1 $ applyToFinal fun e2
    SeqE e1 e2 -> SeqE e1 $ applyToFinal fun e2

-- | Takes an expression, a list of types as argument types and a type as return type 
--   and sets type annotytions of the expression accoringly if it is a function. Otherwise the expression is left unchanged. 
setExprFunType :: Expr ty -> [VarType ty] -> VarType ty -> Expr ty
setExprFunType e argTys retTy = case e of
        (VarE bnd (TypeFunction funTy))       -> VarE bnd (TypeFunction (setFunType argTys retTy funTy))
        (LitE (FunRefLit (FunRef q i funTy))) -> LitE (FunRefLit (FunRef q i (setFunType argTys retTy funTy)))
        -- ToDo: Actually I'd have to ensure, that the return type of body is retTy here
        (LamE pats body)                      -> LamE (zipWith setPatType argTys pats) body
        (TupE fty exprs)                      -> TupE (setFunType argTys retTy fty) exprs
        -- Question: What's the type of BindState at this point?
        other                                 -> other


patterns :: Traversal' (Expr ty) (Pat ty)
patterns f =
    \case
        LamE ps e -> flip LamE e <$> traverse f ps
        LetE p e1 e2 -> (\p' -> LetE p' e1 e2) <$> f p
        o -> pure o

makeBaseFunctor ''Pat

instance Plated (Pat ty) where
    plate f =
        \case
            TupP ps -> TupP <$> traverse f ps
            other -> gplate f other

instance Hashable (Pat ty)

makeBaseFunctor ''Expr

instance Plated (Expr ty) where
    plate f =
        \case
            TupE ty es -> TupE ty <$> traverse f es
            AppE e es -> AppE <$> f e <*> traverse f es
            other -> gplate f other

instance IsList (Expr ty) where
    type Item (Expr ty) = Expr ty
    fromList [] = TupE (FunType [] TypeUnit) []
    fromList (e:exprs) =
        let t = exprType e
            tys = map exprType exprs
        in TupE (FunType (t: tys) (TupleTy (t:|tys))) exprs


-- ToDo: These are currently used in the Lowering tests but actually without a default type (i.e. after
--       removing 'TypeVar') it doen't make to much sense any more to turn strings into pattern or vars
instance IsString (Expr ty) where
    fromString = (\bnd -> VarE bnd TypeNat). fromString

instance IsString (Pat ty) where
    fromString = (\bnd -> VarP bnd TypeNat). fromString

instance IsList (Pat ty) where
    type Item (Pat ty) = (Pat ty)
    fromList (p:ps) = TupP (p:| ps)
    -- Reminder: Depending on where this is needed we could create a WildP from an empty list.
    fromList [] = error $ "Cannot create a tuple pattern from an empty list"
    toList p = error $ "Ohua tried to convert the pattern "
                <>show p <>"into a list, which is not supported"

