{-# LANGUAGE
    TypeOperators
    , DataKinds
    , StandaloneKindSignatures
    , AllowAmbiguousTypes
#-}

module Ohua.Frontend.Lang
    ( Pat(..)
    , Expr(..)
    , UnresolvedExpr
    , ResolvedExpr
    , UnresolvedPat
    , ResolvedPat
    , UnresolvedType
    , ResolvedType
    , patType
    , patBnd
    , patTyBnds
    , freeVars
    , preWalkE
    , preWalkER
    , preWalkMR
    , universeReplace
    , universeReplaceRes
    , universePats
    , flatten
    --- , postWalkE
    ) where

import Ohua.Prelude

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

type Expr :: Type -> Resolution -> F Expr -> Type
data Expr ty res f where
  VarE      :: Binding -> OhuaType ty res                                              -> Expr ty res
  LitE      :: Lit ty res                                                              -> Expr ty res
  LetE      :: Pat ty res -> Expr ty res -> Expr ty res                                -> Expr ty res
  -- We need to add unit arguments and unit parameters to applications and abstractions without args or 
  -- parameters. However, internal types (unit) should only appear in resolved expressions
  -- So there is a unresolved and a resolved version of App and Lam now 
  -- AppEU     :: Expr ty Unresolved -> [Expr ty Unresolved]                              -> Expr ty Unresolved
  AppE      :: Expr ty res -> f (Expr ty res)                         -> Expr ty res
  -- LamEU     :: f (Pat ty Unresolved) -> Expr ty Unresolved                               -> Expr ty Unresolved
  LamE      :: f (Pat ty res) -> Expr ty res                          -> Expr ty res
  IfE       :: Expr ty res -> Expr ty res -> Expr ty res                               -> Expr ty res
  WhileE    :: Expr ty res -> Expr ty res                                              -> Expr ty res
  MapE      :: Expr ty res -> Expr ty res                                              -> Expr ty res
  -- Before BindE consisted of a state expr (a variable) and a method call (AppE)
  -- Now we don't have type annotations at binding sites, so the state can become a Binding in the 'Uresolved'
  -- version
  -- Also, to allow the typecheck to check that the arg types of method include the state, it makes sense to not nest them 
  -- in an AppE expression but keep them directly in the BindE
  -- BindE state function arguments
  -- BindE     :: Expr ty Unresolved -> Binding          -> [Expr ty Unresolved] -> Expr ty Unresolved
  -- The function cannot be just a binding here, because it is an expression in Alang expressions
  -- and we need to "transport" the function type from the type system to the lowering
  -- StateFunE state method args
  StateFunE :: Expr ty res   -> Expr ty res -> f (Expr ty res)   -> Expr ty res
  StmtE     :: Expr ty res -> Expr ty res                                              -> Expr ty res
  TupE      :: NonEmpty (Expr ty res)                                                  -> Expr ty res

-- ToDo: Make Expr a functor without generics 
-- ToDo: Replace clumsy/repetitive traversals
preWalkE :: (Expr ty Unresolved -> Expr ty Unresolved) -> Expr ty Unresolved -> Expr ty Unresolved
preWalkE f e = case e of 
      VarE _ _ -> f e
      LitE _ -> f e 
      LetE p e1 e2 -> 
          let e1' = preWalkE  f e1
              e2' = preWalkE  f e2
          in f (LetE p e1' e2')
      AppEU fun args -> 
          let fun' = preWalkE f fun
              args' = map (preWalkE f) args
          in f (AppEU fun' args')
      LamEU pats body -> 
          let body' = preWalkE f body
          in f (LamEU pats body')
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
      StmtE e1 e2 -> 
          let e1' = preWalkE f e1
              e2' = preWalkE f e2
          in  f (StmtE e1' e2')
      TupE es -> 
          let es' = map (preWalkE f) es
          in  f (TupE es')

preWalkER :: (Expr ty Resolved -> Expr ty Resolved) -> Expr ty Resolved -> Expr ty Resolved
preWalkER f e = case e of 
      VarE _ _ -> f e
      LitE _ -> f e 
      LetE p e1 e2 -> 
          let e1' = preWalkER f e1
              e2' = preWalkER f e2
          in f (LetE p e1' e2')
      AppE fun args -> 
          let fun' = preWalkER f fun
              args' = map (preWalkER f) args
          in f (AppE fun' args')
      LamE pats body -> 
          let body' = preWalkER f body
          in f (LamE pats body')
      IfE c te fe -> 
          let c' = preWalkER f c
              te' = preWalkER f te 
              fe' = preWalkER f fe
          in f (IfE c' te' fe')
      WhileE c body -> 
          let c' = preWalkER f c
              body' = preWalkER f body
          in f (WhileE c' body')
      MapE e1 e2 -> 
          let e1' = preWalkER f e1
              e2' = preWalkER f e2
          in  f (MapE e1' e2')
      StateFunE st meth args ->  
          let st' = preWalkER f st
              meth' = preWalkER f meth
              args' = NE.map (preWalkER f) args
          in f (StateFunE st' meth' args')
      StmtE e1 e2 -> 
          let e1' = preWalkER f e1
              e2' = preWalkER f e2
          in  f (StmtE e1' e2')
      TupE es -> 
          let es' = map (preWalkER f) es
          in  f (TupE es')


preWalkMU :: Monad m =>  (Expr ty Unresolved -> m (Expr ty Unresolved)) -> Expr ty Unresolved -> m (Expr ty Unresolved)
preWalkMU f e = case e of 
      VarE _ _ -> f e
      LitE _ -> f e 
      LetE p e1 e2 ->  do
          e1' <- preWalkMU  f e1
          e2' <- preWalkMU  f e2
          f (LetE p e1' e2')
      AppEU fun args ->  do
          fun' <- preWalkMU f fun
          args' <- mapM (preWalkMU f) args
          f (AppEU fun' args')
      LamEU pats body ->  do
          body' <- preWalkMU f body
          f (LamEU pats body')
      IfE c te fe ->  do
          c' <- preWalkMU f c
          te' <- preWalkMU f te 
          fe' <- preWalkMU f fe
          f (IfE c' te' fe')
      WhileE c body ->  do
          c' <- preWalkMU f c
          body' <- preWalkMU f body
          f (WhileE c' body')
      MapE e1 e2 ->  do
          e1' <- preWalkMU f e1
          e2' <- preWalkMU f e2
          f (MapE e1' e2')
      BindE m sB args ->  do
          m' <- preWalkMU f m
          args' <- mapM (preWalkMU f) args
          f (BindE m' sB args')
      StmtE e1 e2 ->  do
          e1' <- preWalkMU f e1
          e2' <- preWalkMU f e2
          f (StmtE e1' e2')
      TupE es ->  do
          es' <- mapM (preWalkMU f) es
          f (TupE es')

preWalkMR :: Monad m =>  
    (Expr ty Resolved -> m (Expr ty Resolved)) 
    -> Expr ty Resolved 
    -> m (Expr ty Resolved)
preWalkMR f e = case e of 
      VarE _ _ -> f e
      LitE _ -> f e 
      LetE p e1 e2 ->  do
          e1' <- preWalkMR  f e1
          e2' <- preWalkMR  f e2
          f (LetE p e1' e2')
      AppE fun args ->  do
          fun' <- preWalkMR f fun
          args' <- mapM (preWalkMR f) args
          f (AppE fun' args')
      LamE pats body ->  do
          body' <- preWalkMR f body
          f (LamE pats body')
      IfE c te fe ->  do
          c' <- preWalkMR f c
          te' <- preWalkMR f te 
          fe' <- preWalkMR f fe
          f (IfE c' te' fe')
      WhileE c body ->  do
          c' <- preWalkMR f c
          body' <- preWalkMR f body
          f (WhileE c' body')
      MapE e1 e2 ->  do
          e1' <- preWalkMR f e1
          e2' <- preWalkMR f e2
          f (MapE e1' e2')
      StateFunE st meth args ->  do
          meth' <- preWalkMR f meth
          st' <- preWalkMR f st
          args' <- mapM (preWalkMR f) args
          f (StateFunE st' meth' args')
      StmtE e1 e2 ->  do
          e1' <- preWalkMR f e1
          e2' <- preWalkMR f e2
          f (StmtE e1' e2')
      TupE es ->  do
          es' <- mapM (preWalkMR f) es
          f (TupE es')



accu :: Show a => [a] -> a -> [a]
accu l e = l ++ [e] 

patternFromExpr :: Expr ty Resolved -> [Pat ty Resolved]
patternFromExpr e = case e of 
    LamE lPats _body -> NE.toList lPats
    LetE p _e1 _e2 -> [p]
    _e -> [] 

-- preorder list expressions
universeReplace ::  Expr ty Unresolved -> [Expr ty Unresolved]
universeReplace expr =  preWalkMU (accu []) expr

universeReplaceRes ::  Expr ty Resolved -> [Expr ty Resolved]
universeReplaceRes expr =  preWalkMR (accu []) expr

universePats :: Expr ty Resolved -> [Pat ty Resolved]
universePats expr = concatMap patternFromExpr (flatten expr) 

deriving instance Show (Expr ty res)


type UnresolvedExpr ty = Expr ty Unresolved List
type ResolvedExpr ty = Expr ty Resolved List
type FuncExpr ty = Expr ty Resolved NonEmpty

type UnresolvedType ty = OhuaType ty Unresolved
type ResolvedType ty = OhuaType ty Resolved

type UnresolvedPat ty = Pat ty Unresolved
type ResolvedPat ty = Pat ty Resolved

freeVars :: Expr ty res -> [(Binding, OhuaType ty res)]
freeVars = go HS.empty
  where
    go  ctxt (VarE bnd _) | HS.member bnd ctxt = []
    go _ctxt (VarE bnd ty) = [(bnd, ty)]
    go _ctxt (LitE _) = []
    go  ctxt (LetE p e1 e2) = go ctxt e1 ++ (go (foldl (flip HS.insert) ctxt $ patBnd p) e2)
    go  ctxt (AppE f xs) = go ctxt f ++ foldl (\vs e -> vs ++ go ctxt e) [] xs
    go  ctxt (AppEU f xs) = go ctxt f ++ foldl (\vs e -> vs ++ go ctxt e) [] xs
    go  ctxt (LamE ps e) = go (foldl (flip HS.insert) ctxt $ neConcat $ map patBnd ps) e
    go  ctxt (LamEU ps e) =  go (foldl (flip HS.insert) ctxt $ concat $ map (NE.toList . patBnd) ps) e
    go  ctxt (IfE e1 e2 e3) = go ctxt e1 ++ go ctxt e2 ++ go ctxt e3
    go  ctxt (WhileE e1 e2) = go ctxt e1 ++ go ctxt e2
    go  ctxt (MapE e1 e2) = go ctxt e1 ++ go ctxt e2
    go  ctxt (BindE s _ xs) = go ctxt s ++ foldl (\vs e -> vs ++ go ctxt e) [] xs
    -- go  ctxt (StateFunE s _ xs) = go ctxt s ++ foldl (\vs e -> vs ++ go ctxt e) [] xs
    go  ctxt (StateFunE s method args ) = go ctxt s ++ go ctxt method ++ foldl (\vs e -> vs ++ go ctxt e) [] args
    go  ctxt (StmtE e1 e2) = go ctxt e1 ++ go ctxt e2
    go  ctxt (TupE es) = foldl (\vs e -> vs ++ go ctxt e) [] es

flatten :: Expr ty res -> [Expr ty res]
flatten e = case e of 
        (VarE _ _ ) -> [e]
        (LitE _) -> [e]
        (LetE _p e1 e2) -> [e] ++ flatten e1 ++ flatten e2 
        (AppE f xs)-> [e] ++ flatten f ++ concatMap flatten (NE.toList xs)  
        (AppEU f xs)-> [e] ++ flatten f ++ concatMap flatten  xs  
        (LamE _ps lbody)-> [e] ++ flatten lbody  
        (LamEU _ps lbody )->  [e] ++ flatten lbody
        (IfE e1 e2 e3)-> [e] ++ flatten e1 ++ flatten e2 ++ flatten e3
        (WhileE e1 e2)-> [e] ++ flatten e1 ++ flatten e2
        (MapE e1 e2)-> [e] ++ flatten e1 ++ flatten e2
        (BindE s _ xs)-> [e] ++ flatten s ++ concatMap flatten xs 
        (StateFunE s method args)-> [e] ++ flatten s ++ flatten method ++ concatMap flatten (NE.toList args)
        (StmtE e1 e2)-> [e] ++ flatten e1 ++ flatten e2
        (TupE es)-> [e] ++ concatMap flatten es
