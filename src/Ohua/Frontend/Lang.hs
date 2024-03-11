{-# LANGUAGE
    TypeOperators
    , DataKinds
    , StandaloneKindSignatures
    , UndecidableInstances
    , PolyKinds
    , AllowAmbiguousTypes
#-}

module Ohua.Frontend.Lang
    ( Pat(..)
    , Expr(..)
    , MethodRepr(..)
    , UnresolvedExpr
    , ResolvedExpr
    , FuncExpr
    , UnresolvedPat
    , ResolvedPat
    , UnresolvedType
    , ResolvedType
    , patType
    , patBnd
    , patTyBnds
    , exprType
    , freeVars
    , preWalkM
    , preWalkE
    , universeReplace
    , universePats
    , flattenR
    , flattenU
    ) where

import Ohua.Commons.Prelude

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


-- | Expressions in the frontend language are parameterized by
-- 1. The type of embedded host expressions: We need that to wrap e.g. host literals and just annotate them with the type so we don't have to convert
--    every host literal (u8, i32, char, etc.pp into a haskell representation)
-- 2. The actual type of expressions in the given intergration e.g. RustVarType 
-- 3. The resolution to distiguish resolved and unresolved expressions during type checking in the frontend and
-- 4. The type of lists of arguments and parameters because at some point in the frontend, we need to add Unit arguments/parameters 
--    to calls/lambdas to satisfy compiler invariants
type Expr :: Type -> Type -> Type -> Resolution -> (Type -> Type) -> Type
data Expr embExpr annot ty res lists where
  VarE      :: Binding -> OhuaType ty res                                                          -> Expr embExpr annot ty res lists
  LitE      :: Lit embExpr ty res                                                            -> Expr embExpr annot ty res lists
  LetE      :: Pat ty res -> Expr embExpr annot ty res lists -> Expr embExpr annot ty res lists    -> Expr embExpr annot ty res lists
  AppE      :: (Traversable lists) => 
                Expr embExpr annot ty res lists 
                -> [annot] 
                -> lists (Expr embExpr annot ty res lists)                                          -> Expr embExpr annot ty res lists
  LamE      :: (Traversable lists) => lists (Pat ty res) -> Expr embExpr annot ty res lists         -> Expr embExpr annot ty res lists
  IfE       :: Expr embExpr annot ty res lists 
                -> Expr embExpr annot ty res lists 
                -> Expr embExpr annot ty res lists                                                  -> Expr embExpr annot ty res lists
  WhileE    :: Expr embExpr annot ty res lists -> Expr embExpr annot ty res lists                   -> Expr embExpr annot ty res lists
  MapE      :: Expr embExpr annot ty res lists-> Expr embExpr annot ty res lists                    -> Expr embExpr annot ty res lists
  -- The function cannot be just a binding here, because it is an expression in Alang expressions
  -- and we need to "transport" the function type from the type system to the lowering
  -- StateFunE state method args
  StateFunE :: (Traversable lists) 
                => Expr embExpr annot ty res lists 
                -> MethodRepr ty res 
                -> lists (Expr embExpr annot ty res lists)                                          -> Expr embExpr annot ty res lists
  StmtE     :: Expr embExpr annot ty res lists -> Expr embExpr annot ty res lists                   -> Expr embExpr annot ty res lists
  TupE      :: NonEmpty (Expr embExpr annot ty res lists)                                           -> Expr embExpr annot ty res lists


data MethodRepr ty res where
    MethodUnres :: Binding -> MethodRepr ty 'Unresolved
    MethodRes   :: QualifiedBinding -> FunType ty 'Resolved -> MethodRepr ty 'Resolved 

deriving instance Show (MethodRepr ty res)
deriving instance (Show (lists (Expr embExpr annot ty res lists)), Show (lists (Pat ty res)), Show annot) => Show (Expr embExpr annot ty res lists) 

type UnresolvedExpr embExpr annot ty = Expr embExpr annot ty Unresolved [] 
type ResolvedExpr embExpr annot ty = Expr  embExpr annot ty Resolved []
type FuncExpr embExpr annot ty = Expr embExpr annot ty Resolved NonEmpty

type UnresolvedType ty = OhuaType ty Unresolved
type ResolvedType ty = OhuaType ty Resolved

type UnresolvedPat ty = Pat ty Unresolved
type ResolvedPat ty = Pat ty Resolved

preWalkM :: (Monad m, Traversable lists) =>  (Expr embExpr annot ty res lists -> m (Expr embExpr annot ty res lists)) -> Expr embExpr annot ty res lists -> m (Expr embExpr annot ty res lists)
preWalkM f e = case e of 
      VarE _ _ -> f e
      LitE _ -> f e 
      LetE p e1 e2 ->  do
          e1' <- preWalkM  f e1
          e2' <- preWalkM  f e2
          f (LetE p e1' e2')
      AppE fun annots args ->  do
          fun' <- preWalkM f fun
          args' <- mapM (preWalkM f) args
          f (AppE fun' annots args')
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
      StateFunE st meth args -> do 
          -- meth' <- preWalkM f meth
          st' <- preWalkM f st
          args' <- mapM (preWalkM f) args
          f (StateFunE st' meth args')
      StmtE e1 e2 ->  do
          e1' <- preWalkM f e1
          e2' <- preWalkM f e2
          f (StmtE e1' e2')
      TupE es ->  do
          es' <- mapM (preWalkM f) es
          f (TupE es')



accu :: [a] -> a -> [a]
accu l e = l ++ [e] 

{-
 -- Question: Traversing traversables gives Elements: How to get the actual type i.e.
    Pat ty res from Element (lists (Pat ty res))???
    
    patternFromExpr ::(Traversable lists) =>  Expr embExpr annot ty res lists ->  [Pat ty res]
    patternFromExpr e = case e of 
    LamE lPats _body -> toList lPats
    LetE p _e1 _e2 -> [p]
    _e -> []


patternFromUExpr :: Expr ty res [] ->  [Pat ty res]
patternFromUExpr e = case e of 
    LamE lPats _body -> lPats
    LetE p _e1 _e2 -> [p]
    _e -> []
-}

-- | We need this function to insert sequencing (formerly known as Seq-expression) before lowering to Alang
exprType ::(Show annot) => FuncExpr embExpr annot ty -> ResolvedType ty
exprType = \case
    (VarE _b ty) -> ty
    (LitE  lit) -> getLitType lit
    (LetE _p _e cont) -> exprType cont
    (AppE fun _annot _args) -> returnType fun
    (LamE ps term) -> FType $ FunType (Right $ map patType ps) (exprType term)
    (IfE c t1 t2) -> exprType t1 
    (WhileE c body) -> IType TypeUnit 
    -- The return of a map (t1->t2) (c t1) should be (c t2), currently we expect any container to show list like behaviour 
    -- and therefor will use the internal list type to represent c 
    (MapE fun container) -> IType $ TypeList (returnType fun)
    (StmtE e1 e2) -> IType TypeUnit
    (StateFunE _st (MethodRes _ fty) _args) -> getReturnType (FType fty)
    (TupE es) -> TType (NE.map exprType es)

returnType :: (Show annot) => FuncExpr embExpr annot ty -> ResolvedType ty
returnType e = case funType e of
    Just fty -> getReturnType fty
    Nothing -> exprType e

funType :: (Show annot) => FuncExpr embExpr annot ty -> Maybe (ResolvedType ty)
funType e = case e of
        (VarE _bnd (FType fTy))          -> Just (FType fTy)
        (LitE (FunRefLit fRef))          -> Just $ FType (getRefType fRef)
        (LamE pats body)                 -> Just $ FType (FunType (Right $ map patType pats) (exprType body))
        other                            -> trace ("funtype called with " <> show other) Nothing

   

patternFromExpr :: Expr embExpr annot ty res NonEmpty ->  [Pat ty res]
patternFromExpr e = case e of 
    LamE lPats _body -> NE.toList lPats
    LetE p _e1 _e2 -> [p]
    _e -> []

-- preorder list expressions
universeReplace :: (Traversable lists) => Expr embExpr annot ty res lists -> [Expr embExpr annot ty res lists]
universeReplace expr =  preWalkM (accu []) expr

{-
universeReplaceRes ::  Expr ty Resolved -> [Expr ty Resolved]
universeReplaceRes expr =  preWalkMR (accu []) expr
-}

-- We currently only use this with resolved (Func) expressions
universePats :: Expr embExpr annot ty res NonEmpty -> [Pat ty res]
universePats expr = concatMap patternFromExpr (flattenR expr) 

-- We currently only use this with unresolved expressions
freeVars :: Expr embExpr annot ty res [] -> [(Binding, OhuaType ty res)]
freeVars e = go HS.empty e
  where
    go :: HS.HashSet Binding -> Expr embExpr annot ty res [] -> [(Binding, OhuaType ty res)]
    go  ctxt (VarE bnd _) | HS.member bnd ctxt = []
    go _ctxt (VarE bnd ty) = [(bnd, ty)]
    go _ctxt (LitE _) = []
    go  ctxt (LetE p e1 e2) = go ctxt e1 ++ (go (foldl (flip HS.insert) ctxt $ patBnd p) e2)
    go  ctxt (AppE f annots xs) = go ctxt f ++ foldl (\vs e1 -> vs ++ go ctxt e1) [] xs
    go  ctxt (LamE ps e1) = go (foldl (flip HS.insert) ctxt $ concat $ map (NE.toList . patBnd) ps) e1
    go  ctxt (IfE e1 e2 e3) = go ctxt e1 ++ go ctxt e2 ++ go ctxt e3
    go  ctxt (WhileE e1 e2) = go ctxt e1 ++ go ctxt e2
    go  ctxt (MapE e1 e2) = go ctxt e1 ++ go ctxt e2
    -- Question: Can a method be a free variable ? 
    go  ctxt (StateFunE s _method args ) = go ctxt s {-++ go ctxt method -} ++ foldl (\vs e1 -> vs ++ go ctxt e1) [] args
    go  ctxt (StmtE e1 e2) = go ctxt e1 ++ go ctxt e2
    go  ctxt (TupE es) = foldl (\vs e1 -> vs ++ go ctxt e1) [] es

-- FIXME: Same problem as with pattern extraction. Making the function generic over lists 
--        turns the type of elements into Elements and I don't know how to handle them
flattenU :: Expr embExpr annot ty res [] -> [Expr embExpr annot ty res []]
flattenU e = case e of 
        (VarE _ _ ) -> [e]
        (LitE _) -> [e]
        (LetE _p e1 e2) -> [e] ++ flattenU e1 ++ flattenU e2 
        (AppE f annots xs)-> [e] ++ flattenU f ++ concatMap flattenU xs  
        -- (AppEU f xs)-> [e] ++ flattenU f ++ concatMap flattenU  xs  
        (LamE _ps lbody)-> [e] ++ flattenU lbody  
        -- (LamEU _ps lbody )->  [e] ++ flattenU lbody
        (IfE e1 e2 e3)-> [e] ++ flattenU e1 ++ flattenU e2 ++ flattenU e3
        (WhileE e1 e2)-> [e] ++ flattenU e1 ++ flattenU e2
        (MapE e1 e2)-> [e] ++ flattenU e1 ++ flattenU e2
        -- (BindE s _ xs)-> [e] ++ flattenU s ++ concatMap flattenU xs 
        -- Reminder: We take the methods out of the expression list here. That will require changes in
        --           the way code using this function acts on StateFunE
        (StateFunE s _method args)-> [e] ++ flattenU s {- ++ flattenU method -} ++ concatMap flattenU args
        (StmtE e1 e2)-> [e] ++ flattenU e1 ++ flattenU e2
        (TupE es)-> [e] ++ concatMap flattenU es

flattenR :: Expr embExpr annot ty res NonEmpty -> [Expr embExpr annot ty res NonEmpty]
flattenR e = case e of 
        (VarE _ _ ) -> [e]
        (LitE _) -> [e]
        (LetE _p e1 e2) -> [e] ++ flattenR e1 ++ flattenR e2 
        (AppE f a xs)-> [e] ++ flattenR f ++ concatMap flattenR xs  
        -- (AppEU f xs)-> [e] ++ flattenR f ++ concatMap flattenR  xs  
        (LamE _ps lbody)-> [e] ++ flattenR lbody  
        -- (LamEU _ps lbody )->  [e] ++ flattenR lbody
        (IfE e1 e2 e3)-> [e] ++ flattenR e1 ++ flattenR e2 ++ flattenR e3
        (WhileE e1 e2)-> [e] ++ flattenR e1 ++ flattenR e2
        (MapE e1 e2)-> [e] ++ flattenR e1 ++ flattenR e2
        -- (BindE s _ xs)-> [e] ++ flattenR s ++ concatMap flattenR xs 
        (StateFunE s _method args)-> [e] ++ flattenR s {- ++ flattenR method-} ++ concatMap flattenR args
        (StmtE e1 e2)-> [e] ++ flattenR e1 ++ flattenR e2
        (TupE es)-> [e] ++ concatMap flattenR es

preWalkE :: (Traversable lists) => (Expr embExpr annot ty res lists -> Expr embExpr annot ty res lists) -> Expr embExpr annot ty res lists -> Expr embExpr annot ty res lists
preWalkE f e = case e of 
      VarE _ _ -> f e
      LitE _ -> f e 
      LetE p e1 e2 -> 
          let e1' = preWalkE  f e1
              e2' = preWalkE  f e2
          in f (LetE p e1' e2')
      AppE fun annots args -> 
          let fun' = preWalkE f fun
              args' = map (preWalkE f) args
          in f (AppE fun' annots args')
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
      StateFunE st meth args ->
          let st' = preWalkE f st
              args' = map (preWalkE f) args
          in f (StateFunE st' meth args')
      StmtE e1 e2 -> 
          let e1' = preWalkE f e1
              e2' = preWalkE f e2
          in  f (StmtE e1' e2')
      TupE es -> 
          let es' = map (preWalkE f) es
          in  f (TupE es')
