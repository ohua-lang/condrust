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
    , patType
    , patBnd
    , patTyBnds
    , unitArgs
    , unitParams
    , freeVars
    , preWalkE
    , universeReplace
    --- , postWalkE
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
patType :: Pat ty res -> OhuaType ty res
patType (VarP _ ty) = ty
patType (TupP ps) = TType (map patType ps)

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
  LitE      :: Lit ty res                                                              -> Expr ty res
  LetE      :: Pat ty res -> Expr ty res -> Expr ty res                                -> Expr ty res
  AppE      :: Expr ty res -> NonEmpty (Expr ty res)                                   -> Expr ty res
  LamE      :: NonEmpty (Pat ty res) -> Expr ty res                                    -> Expr ty res
  IfE       :: Expr ty res -> Expr ty res -> Expr ty res                               -> Expr ty res
  WhileE    :: Expr ty res -> Expr ty res                                              -> Expr ty res
  MapE      :: Expr ty res -> Expr ty res                                              -> Expr ty res
  -- Before BindE consisted of a state expr (a variable) and a method call (AppE)
  -- Now we don't have type annotations at binding sites, so the state can become a Binding in the 'Uresovled'
  -- version
  -- Also, to allow the typecheck to check that the arg types of method include the state, it makes sense to not nest them 
  -- in an AppE expression but keep them directly in the BindE
  BindE     :: Expr ty Unresolved -> Binding          -> NonEmpty (Expr ty Unresolved) -> Expr ty Unresolved
  -- However I don't see why in StateFunE we should 
  --     a) keep the QualifiedBinding instead of the VarE the state should be at this point -> maybe to enforce it ... well ok then 
  --     b) keep the args in the StateFunE Expression instead of nesting it in an AppE again
  -- StateFunE :: Expr ty Resolved   -> QualifiedBinding -> NonEmpty (Expr ty Resolved)   -> Expr ty Resolved
  --           State VarE             fullName           method call
  StateFunE :: Expr ty Resolved   -> QualifiedBinding -> Expr ty Resolved              -> Expr ty Resolved
  StmtE     :: Expr ty res -> Expr ty res                                              -> Expr ty res
  TupE      :: NonEmpty (Expr ty res)                                                  -> Expr ty res

-- ToDo: Make Expr a functor without generics 

preWalkE :: (Expr ty Unresolved -> Expr ty Unresolved) -> Expr ty Unresolved -> Expr ty Unresolved
preWalkE f e = case e of 
      VarE _ _ -> f e
      LitE _ -> f e 
      LetE p e1 e2 -> 
          let e1' = preWalkE  f e1
              e2' = preWalkE  f e2
          in f (LetE p e1' e2')
      AppE fun args -> 
          let fun' = preWalkE f fun
              args' = map (preWalkE f) args
          in f (AppE fun' args')
      LamE pats body -> 
          let body' = preWalkE f body
          in f (LamE pats body')
      IfE c te fe -> 
          let c' = preWalkE f c
              te' = preWalkE f te 
              fe' = preWalkE f fe
          in f (IfE c' te' fe')
      WhileE c body -> 
          let c' = preWalkE f c
              body' = preWalkE f body
          in f (WhileE c' body')
      MapE e1 e2 -> 
          let e1' = preWalkE f e1
              e2' = preWalkE f e2
          in  f (MapE e1' e2')
      BindE m sB args -> 
          let m' = preWalkE f m
              args' = map (preWalkE f) args
          in f (BindE m' sB args')
      {-StateFunE s sb m -> 
          let m' = preWalkE f m
              s' = preWalkE f s
          in f (StateFunE s' sb m')-}
      StmtE e1 e2 -> 
          let e1' = preWalkE f e1
              e2' = preWalkE f e2
          in  f (StmtE e1' e2')
      TupE es -> 
          let es' = map (preWalkE f) es
          in  f (TupE es')
           
      


preWalkM :: Monad m =>  (Expr ty Unresolved -> m (Expr ty Unresolved)) -> Expr ty Unresolved -> m (Expr ty Unresolved)
preWalkM f e = case e of 
      VarE _ _ -> f e
      LitE _ -> f e 
      LetE p e1 e2 ->  do
          e1' <- preWalkM  f e1
          e2' <- preWalkM  f e2
          f (LetE p e1' e2')
      AppE fun args ->  do
          fun' <- preWalkM f fun
          args' <- mapM (preWalkM f) args
          f (AppE fun' args')
      LamE pats body ->  do
          body' <- preWalkM f body
          f (LamE pats body')
      IfE c te fe ->  do
          c' <- preWalkM f c
          te' <- preWalkM f te 
          fe' <- preWalkM f fe
          f (IfE c' te' fe')
      WhileE c body ->  do
          c' <- preWalkM f c
          body' <- preWalkM f body
          f (WhileE c' body')
      MapE e1 e2 ->  do
          e1' <- preWalkM f e1
          e2' <- preWalkM f e2
          f (MapE e1' e2')
      BindE m sB args ->  do
          m' <- preWalkM f m
          args' <- mapM (preWalkM f) args
          f (BindE m' sB args')
      {-StateFunE s sb m ->  do
          m' <- preWalkM f m
              s' <- preWalkM f s
          in f (StateFunE s' sb m')-}
      StmtE e1 e2 ->  do
          e1' <- preWalkM f e1
          e2' <- preWalkM f e2
          f (StmtE e1' e2')
      TupE es ->  do
          es' <- mapM (preWalkM f) es
          f (TupE es')

{-postwalk can fail if f is not structure preserving -> how to? 

-> get a subExpr and compose function for exprs 
-> handle expr 
-> get and handle subexprs
-> compose -> return 
postWalkE ::Monad m =>  (Expr ty Unresolved -> m (Expr ty Unresolved)) -> Expr ty Unresolved -> m (Expr ty Unresolved)
postWalkE f e = case e of 
      VarE _ _ -> f e
      LitE _ -> f e 
      LetE p e1 e2 -> do
           (LetE p' e1' e2') <- f e
           e1'' <- postWalkE f e1'
           e2'' <- postWalkE  f e2'
           return (LetE p' e1'' e2'')
      AppE fun args ->  do
            (AppE fun' args') <- f e
            fun'' <- postWalkE f fun'
            args'' <- mapM (postWalkE f) args'
            return (AppE fun'' args'')
      LamE pats body -> do
            (LamE pats' body') <- f e 
            body'' <- postWalkE f body'
            return (LamE pats' body'')
      IfE c te fe ->  do
            (IfE c' te' fe') <- f e 
            c'' <- postWalkE f c'
            te'' <- postWalkE f te' 
            fe'' <- postWalkE f fe'
            return (IfE c'' te'' fe'')
      WhileE c body ->  do
            (WhileE c' body') <- f e
            c'' <- postWalkE f c'
            body'' <- postWalkE f body'
            return (WhileE c'' body'')
      MapE e1 e2 ->  do
            (MapE e1' e2') <- f e 
            e1'' <- postWalkE f e1'
            e2'' <- postWalkE f e2'
            return (MapE e1'' e2'')
      BindE m sB args ->  do
            (BindE m' sB' args') <- f e 
            m'' <- postWalkE f m'
            args'' <- mapM (postWalkE f) args'
            return (BindE m'' sB' args'')
      {-StateFunE s sb m ->  do
          m' <- postWalkE f m
              s' <- postWalkE f s
          in f (StateFunE s' sb m')-}
      StmtE e1 e2 ->  do
           (StmtE e1' e2') <- f e 
           e1'' <- postWalkE f e1'
           e2'' <- postWalkE f e2'
           return  (StmtE e1'' e2'')
      TupE es ->  do
            (TupE es') <- f e 
            es'' <- mapM (postWalkE f) es'
            return (TupE es'')
-}

accu :: [a] -> a -> [a]
accu l e = l ++ [e] 


-- preorder list expressions
universeReplace ::  Expr ty Unresolved -> [Expr ty Unresolved]
universeReplace expr =  preWalkM (accu []) expr

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
    -- go  ctxt (StateFunE s _ xs) = go ctxt s ++ foldl (\vs e -> vs ++ go ctxt e) [] xs
    go  ctxt (StateFunE s _ method ) = go ctxt s ++ go ctxt method
    go  ctxt (StmtE e1 e2) = go ctxt e1 ++ go ctxt e2
    go  ctxt (TupE es) = foldl (\vs e -> vs ++ go ctxt e) [] es
