{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Ohua.Frontend.Lang
    ( Pat(..)
    , Expr(..)
--    , exprType
--    , returnType
--    , funType
    , patType
    , patBnd
    , patTyBnds
--    , setPatType
--    , setExprFunType
    , PatF(..)
    , ExprF(..)
    , patterns
--    , applyToFinal
    ) where

import Ohua.UResPrelude


import Control.Lens (Traversal')
import Control.Lens.Plated (Plated, gplate, plate)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import qualified Data.List.NonEmpty as NE
import GHC.Exts

data Pat ty
    = VarP Binding (VarType ty)
    | TupP (NonEmpty (Pat ty))
    -- | WildP -- (VarType ty)
    deriving (Show, Eq, Generic)

patType :: Pat ty -> VarType ty
patType = \case
    VarP _ ty -> ty
    TupP ps -> TupleTy (map patType ps)

patBnd :: Pat ty -> NonEmpty Binding
patBnd = \case
    VarP bnd ty -> bnd :| []
    TupP (ps) -> neConcat $ map patBnd ps

patTyBnds :: Pat ty -> NonEmpty (Binding, VarType ty)
patTyBnds = \case
    VarP bnd ty -> (bnd, ty) :| []
    TupP (ps) -> neConcat $ map patTyBnds ps

{-
setPatType :: VarType ty -> Pat ty -> Pat ty
setPatType nty = \case
    VarP bnd ty -> VarP bnd nty
    TupP ps -> case nty of
        TupleTy tys -> TupP $ NE.map (uncurry setPatType) (NE.zip tys ps)
    WildP ty -> WildP nty
-}
{-

Again this approach does not work because of all this TH non-sense in the
traversal libraries that we use.
Again, let's find a way to get rid of Haskell soon.

data Expr' varF ty
    -- REMINDER: We need to wrap the host type in an VarType here, because
    -- the compiler will introdude variables typed as internal bool/unit/int 
    -- ... that also have to be representable
    = VarE Binding (varF ty)
    | LitE (Lit ty)
    | LetE (Pat ty)
           (Expr' varF ty)
           (Expr' varF ty)
    | AppE (Expr' varF ty)
           [Expr' varF ty]
    | LamE [Pat ty]
           (Expr' varF ty) -- ^ An expression creating a function
    | IfE (Expr' varF ty)
          (Expr' varF ty)
          (Expr' varF ty)
    | WhileE (Expr' varF ty) (Expr' varF ty)
    | MapE (Expr' varF ty) -- ^ Map expression that 'maps' its first argument to its second :: map f xs.
           (Expr' varF ty)

    | BindE (Expr' varF ty)
            (Expr' varF ty) -- ^ @BindE state function@ binds @state@ to be operated on by @function@
    | StmtE (Expr' varF ty)
            (Expr' varF ty) -- ^ An expression with the return value ignored
    | SeqE (Expr' varF ty)
           (Expr' varF ty)
    | TupE (NonEmpty (Expr' varF ty)) -- ^ create a tuple value that can be destructured
    deriving (Show, Generic)

type Expr ty = Expr (Expr' VarType ty)
type UnresolvedExpr ty = UnresolvedExpr (Expr' UnresolvedVarType ty)

--}

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
           (NonEmpty (Expr ty))
    | LamE (NonEmpty (Pat ty))
           (Expr ty) -- ^ An expression creating a function
    | IfE (Expr ty)
          (Expr ty)
          (Expr ty)
    | WhileE (Expr ty)
             (Expr ty)
    | MapE (Expr ty) -- ^ Map expression that 'maps' its first argument to its second :: map f xs.
           (Expr ty)
    | BindE (Expr ty)
            (Expr ty) -- ^ @BindE state function@ binds @state@ to be operated on by @function@
    | StmtE (Expr ty)
            (Expr ty) -- ^ An expression with the return value ignored
--    | SeqE (Expr ty)
--           (Expr ty)
    | TupE (NonEmpty (Expr ty)) -- ^ create a tuple value that can be destructured
    deriving (Show, Generic)
{-
exprType :: Expr ty -> VarType ty
exprType (VarE _b ty) = ty
exprType (LitE  lit) = getLitType lit
exprType (LetE _p _e cont) = exprType cont
exprType (AppE fun args) = returnType fun
-- Type of an abstraction (\x:T1. term:T2) as generics are a 'problem' of the host language
-- we do not handle any subtitution here and just take the return type of the term
exprType (LamE ps term) = TypeFunction $ FunType (map patType ps) (exprType term)
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
exprType (TupE exprs ) = TupleTy (map exprType exprs)

returnType :: Expr ty -> VarType ty
returnType e = case funType e of
    Just fty -> getReturnType fty
    Nothing -> exprType e

funType :: Expr ty -> Maybe (FunType ty)
funType e = case e of
        (VarE _bnd (TypeFunction fTy))   -> Just fTy
        (LitE (FunRefLit fRef))          -> Just $ getRefType fRef
        (LamE pats body)                 -> Just $ FunType (map patType pats) (exprType body)
        (BindE _state method)            -> funType method
        -- Question: What's the type of BindState at this point?
        other                            -> trace ("funtype called with " <> show other) Nothing


-- If expressions have two possible exits
-- Question how to make this more elegant. It's not a normal traversal as we don't want to recurse
-- into every nested expression?
applyToFinal ::(Monad m) =>  (Expr ty -> m (Expr ty)) -> Expr ty -> m (Expr ty)
applyToFinal fun =  \case
    e@VarE{} -> fun e
    e@LitE{} -> fun e
    e@TupE{} -> fun e
    LetE p e1 e2 -> LetE p e1 <$> applyToFinal fun e2
    AppE f args -> (`AppE` args) <$> applyToFinal fun f
    LamE p e -> LamE p <$> applyToFinal fun e
    IfE b e1 e2 -> IfE b <$> applyToFinal fun e1 <*> applyToFinal fun e2
    WhileE e1 e2 -> WhileE e1 <$> applyToFinal fun e2
    MapE e1 e2 -> (`MapE` e2) <$> applyToFinal fun e1 -- we map a function to a container so the final return is the return of the function
    BindE e1 e2 -> BindE e1 <$> applyToFinal fun e2
    StmtE e1 e2 -> StmtE e1 <$> applyToFinal fun e2
    SeqE e1 e2 -> SeqE e1 <$> applyToFinal fun e2

-- | Takes an expression, a list of types as argument types and a type as return type 
--   and sets type annotytions of the expression accoringly if it is a function. Otherwise the expression is left unchanged. 
setExprFunType :: Expr ty -> [VarType ty] -> VarType ty -> Expr ty
setExprFunType e argTys retTy = case e of
        (VarE bnd (TypeFunction funTy))       -> VarE bnd (TypeFunction (setFunType argTys retTy funTy))
        (LitE (FunRefLit (FunRef q i funTy))) -> LitE (FunRefLit (FunRef q i (setFunType argTys retTy funTy)))
        -- ToDo: Actually I'd have to ensure, that the return type of body is retTy here
        (LamE pats body)                      -> LamE (zipWith setPatType argTys pats) body
        -- (TupE exprs)                          -> TupE exprs
        -- Question: What's the type of BindState at this point?
        other                                 -> other
-}

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
            TupE es -> TupE <$> traverse f es
            AppE e es -> AppE <$> f e <*> traverse f es
            other -> gplate f other

instance IsList (Expr ty) where
    type Item (Expr ty) = Expr ty
    fromList [] =  LitE UnitLit
    fromList (e:es) = TupE (e:| es)


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

