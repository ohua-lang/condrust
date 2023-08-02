module Ohua.Frontend.Transform.Scopedness where

import Ohua.Prelude

import Ohua.Frontend.Lang
import Ohua.Frontend.PPrint ()
import Data.HashSet as HS

import qualified Data.List.NonEmpty as NE
import qualified Data.Traversable as TR



type Ctxt = HS.HashSet Binding

contextedTraversal :: Monad m => (Ctxt -> UnresolvedExpr ty -> m (UnresolvedExpr ty)) -> Ctxt -> UnresolvedExpr ty -> m (UnresolvedExpr ty )
contextedTraversal f = go
    where
        --  see example 5 in
        -- https://ndmitchell.com/downloads/paper-uniform_boilerplate_and_list_processing-30_sep_2007.pdf
        -- go:: Ctxt -> ResolvedExpr ty -> m (ResolvedExpr ty ) 
        go ctxt (LetE v a b) =
            let ctxt' = HS.union ctxt $ HS.fromList $ goPat v
            in LetE v <$> go ctxt' a -- take recursive calls into account
                      <*> go ctxt' b
        go ctxt (LamE vs b) =
            let ctxt' = HS.union ctxt $ HS.fromList $ join $ Ohua.Prelude.map goPat $ Ohua.Prelude.toList vs
            in LamE vs <$> go ctxt' b
        go ctxt v@(VarE bdg ty) | not (HS.member bdg ctxt) = f ctxt v
        -- Question: Would we ever expect this to happen?
        go _ctxt v@(VarE _ _) = return v
        go _ctxt l@(LitE _ ) = return l
        -- descendM = mapMOf plate -- http://hackage.haskell.org/package/lens-3.0.6/docs/Control-Lens-Plated.html#v:descendM
        -- Recurse one level into a structure with a monadic effect. (a.k.a composOpM from Björn Bringert's compos) 
        -- So replace decendM with bacially further decending because go intself is recursive
        -- ToDo: Make Expr Functor/Applicative again ? -> Different for 'Resolved/'Unresolved
        go ctxt (AppE fe args) = AppE <$> go ctxt fe <*> TR.mapM (go ctxt) args  
        go ctxt (IfE c et ef) = IfE <$> go ctxt c <*> go ctxt et <*> go ctxt ef
        go ctxt (WhileE c body) = WhileE <$> go ctxt c <*> go ctxt body
        go ctxt (MapE fe gen) = MapE <$> go ctxt fe <*> go ctxt gen 
        go ctxt (BindE m s args) = flip BindE s <$> go ctxt m <*> TR.mapM (go ctxt) args  
        -- go ctxt (StateFunE fe qB args) = flip StateFunE qB <$> go ctxt fe <*> TR.mapM (go ctxt) args
        -- go ctxt (StateFunE s qB mCall) = flip StateFunE qB <$> go ctxt s <*> go ctxt mCall
        go ctxt (StmtE e1 e2) = StmtE <$> go ctxt e1 <*> go ctxt e2
        go ctxt (TupE es) = TupE <$> TR.mapM (go ctxt) es

        -- goPat:: 
        goPat (VarP bdg _pTy) = [bdg]
        goPat (TupP ps) = join $ Ohua.Prelude.map goPat (NE.toList ps)
{-
-- | This transformation establishes well-scopedness
--      The implementation is quite straight forward: collect the variables and check for
--      references that are not defined as variables (usually in function position).
--      These must be references to functions in the current namespace.
makeWellScoped :: Binding -> Expr ty -> Expr ty
makeWellScoped currentFun expr = runIdentity $ contextedTraversal intoFunLit (HS.singleton currentFun) expr
  where
    intoFunLit _ (VarE bdg (TypeFunction fTy)) = return $ LitE $ FunRefLit $ FunRef (QualifiedBinding (makeThrow []) bdg) Nothing fTy
    intoFunLit _ (VarE bnd TypeVar) = return $ LitE $ FunRefLit $ FunRef (QualifiedBinding (makeThrow []) bdg) Nothing $ FunType [TypeVar] TypeVar
    intoFunLit _ e = return e
-}
