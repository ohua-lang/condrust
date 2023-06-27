module Ohua.Frontend.Transform.Scopedness where

import Ohua.Prelude

import Ohua.Frontend.Lang
import Ohua.Frontend.PPrint ()
import Data.HashSet as HS
import Control.Lens.Combinators (mapMOf)
import Control.Lens.Plated (plate)
import qualified Data.List.NonEmpty as NE


type Ctxt = HS.HashSet Binding

contextedTraversal :: Monad m => (Ctxt -> Expr ty -> m (Expr ty)) -> Ctxt -> Expr ty -> m (Expr ty)
contextedTraversal f = go
    where
        --  see example 5 in
        -- https://ndmitchell.com/downloads/paper-uniform_boilerplate_and_list_processing-30_sep_2007.pdf
        go ctxt (LetE v a b) =
            let ctxt' = HS.union ctxt $ HS.fromList $ goPat v
            in LetE v <$> go ctxt' a -- take recursive calls into account
                      <*> go ctxt' b
        go ctxt (LamE vs b) =
            let ctxt' = HS.union ctxt $ HS.fromList $ join $ Ohua.Prelude.map goPat vs
            in LamE vs <$> go ctxt' b
        go ctxt v@(VarE bdg ty) | not (HS.member bdg ctxt) = f ctxt v
        go ctxt e = descendM (go ctxt) e
        descendM = mapMOf plate -- http://hackage.haskell.org/package/lens-3.0.6/docs/Control-Lens-Plated.html#v:descendM
        --descend = over plate -- note composOp = descend = over plate -> https://www.stackage.org/haddock/lts-14.25/lens-4.17.1/Control-Lens-Plated.html#v:para (below)
        goPat (WildP _ty) = []
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
