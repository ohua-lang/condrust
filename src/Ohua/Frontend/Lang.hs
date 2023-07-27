{-# LANGUAGE
    TypeOperators
    , DataKinds
    , StandaloneKindSignatures
    , AllowAmbiguousTypes
#-}

module Ohua.Frontend.Lang
    ( Pat(..)
    , Expr(..)
    , UnresolvedExpr(..)
    , ResolvedExpr(..)
    , UnresolvedPat
    , ResolvedPat
    , UnresolvedType
    , ResolvedType
--    , exprType
--    , returnType
--    , funType
    , patType
    , patBnd
    , patTyBnds
--    , setPatType
--    , setExprFunType
--    , PatF(..)
--    , ExprF(..)
--    , patterns
--    , applyToFinal
    , unitArgs
    , unitParams
    , freeVars
    ) where

import Ohua.UResPrelude

import qualified Data.List.NonEmpty as NE
import qualified Data.HashSet as HS


type Pat :: Type -> Resolution -> Type
data Pat ty res where
  VarP :: Binding -> OhuaType ty res -> Pat ty res
  TupP :: NonEmpty (Pat ty res) -> Pat ty res
    
deriving instance Show (Pat ty res)
instance Heq (Pat ty res1) (Pat ty res2) where
  heq (VarP b1 _) (VarP b2 _) = b1 == b2
  heq (TupP xs1) (TupP xs2) =
    NE.length xs1 == NE.length xs2 &&
    (and $ map (uncurry heq) $ NE.zip xs1 xs2)
  heq _ _ = False

-- FIXME: There's a problem wtih TupleTy .. on the hand it should be an internal Type i.e. Resolved, on the other hand we use it to represent 
--        tuples of HostTypes and Unresoved  :-/ .. We might need another representation for Tuples of host types
patType :: Pat ty 'Resolved -> OhuaType ty 'Resolved
patType (VarP _ ty) = ty
patType (TupP ps) = IType $ TupleTy (map patType ps)

patBnd :: Pat ty res -> NonEmpty Binding
patBnd = \case
    VarP bnd _ty -> bnd :| []
    TupP (ps) -> neConcat $ map patBnd ps

patTyBnds :: Pat ty res -> NonEmpty (Binding, OhuaType ty res)
patTyBnds = \case
    VarP bnd ty -> (bnd, ty) :| []
    TupP (ps) -> neConcat $ map patTyBnds ps

type Expr :: Type -> Resolution -> Type
data Expr ty res where
  VarE      :: Binding -> OhuaType ty res                                              -> Expr ty res
  LitE      :: Lit ty reservedNames                                                    -> Expr ty res
  LetE      :: Pat ty res -> Expr ty res -> Expr ty res                                -> Expr ty res
  AppE      :: Expr ty res -> NonEmpty (Expr ty res)                                   -> Expr ty res
  LamE      :: NonEmpty (Pat ty res) -> Expr ty res                                    -> Expr ty res
  IfE       :: Expr ty res -> Expr ty res -> Expr ty res                               -> Expr ty res
  WhileE    :: Expr ty res -> Expr ty res                                              -> Expr ty res
  MapE      :: Expr ty res -> Expr ty res                                              -> Expr ty res
  BindE     :: Expr ty Unresolved -> Binding          -> NonEmpty (Expr ty Unresolved) -> Expr ty Unresolved
  StateFunE :: Expr ty Resolved   -> QualifiedBinding -> NonEmpty (Expr ty Resolved)   -> Expr ty Resolved
  StmtE     :: Expr ty res -> Expr ty res                                              -> Expr ty res
  TupE      :: NonEmpty (Expr ty res)                                                  -> Expr ty res

deriving instance Show (Expr ty res)

type UnresolvedExpr ty = Expr ty Unresolved
type ResolvedExpr ty = Expr ty Resolved

type UnresolvedType ty = OhuaType ty Unresolved
type ResolvedType ty = OhuaType ty Resolved

type UnresolvedPat ty = Pat ty Unresolved
type ResolvedPat ty = Pat ty Resolved

unitArgs :: [Expr ty res] -> NonEmpty (Expr ty res)
unitArgs []     = LitE UnitLit :| []
unitArgs (x:xs) = x :| xs


unitParams :: [Pat ty Resolved] -> NonEmpty (Pat ty Resolved)
unitParams []     = (VarP "_" $ IType TypeUnit) :| []
unitParams (x:xs) = x :| xs


freeVars :: Expr ty res -> [(Binding, OhuaType ty res)]
freeVars = go HS.empty
  where
    go  ctxt (VarE bnd _) | HS.member bnd ctxt = []
    go _ctxt (VarE bnd ty) = [(bnd, ty)]
    go  ctxt (LitE _) = []
    go  ctxt (LetE p e1 e2) = go ctxt e1 ++ (go (foldl (flip HS.insert) ctxt $ patBnd p) e2)
    go  ctxt (AppE f xs) = go ctxt f ++ foldl (\vs e -> vs ++ go ctxt e) [] xs
    go  ctxt (LamE ps e) = go (foldl (flip HS.insert) ctxt $ neConcat $ map patBnd ps) e
    go  ctxt (IfE e1 e2 e3) = go ctxt e1 ++ go ctxt e2 ++ go ctxt e3
    go  ctxt (WhileE e1 e2) = go ctxt e1 ++ go ctxt e2
    go  ctxt (MapE e1 e2) = go ctxt e1 ++ go ctxt e2
    go  ctxt (BindE s _ xs) = go ctxt s ++ foldl (\vs e -> vs ++ go ctxt e) [] xs
    go  ctxt (StateFunE s _ xs) = go ctxt s ++ foldl (\vs e -> vs ++ go ctxt e) [] xs
    go  ctxt (StmtE e1 e2) = go ctxt e1 ++ go ctxt e2
    go  ctxt (TupE es) = foldl (\vs e -> vs ++ go ctxt e) [] es
