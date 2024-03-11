{-|

Module      : $Header$
Description : Transformation for splicing in algos for algo references.
Copyright   : (c) Sebastian Ertel 2020. All Rights Reserved.
License     : OtherLicense
Maintainer  : sebastian.ertel@gmail.com
Stability   : experimental
Portability : portable
This source code is licensed under the terms described in the associated LICENSE.TXT file

This module is a transformation that turns a function reference into an expression.
The resulting expression does not contain any (function reference) anymore.
Function references in this transformation are just references to variable that refer to
a function. That is, this transformation does not deal with namespaces or such.

-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Frontend.Transform.Resolve where

import Ohua.Commons.Prelude

import Ohua.Frontend.Lang as FrLang (Expr(..), UnresolvedExpr, Pat(VarP), flattenU, preWalkE)
import Ohua.Frontend.Types
import Ohua.Frontend.PPrint ()

import Control.Lens (over)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T

-- REMINDER: Algo Inlining happens here. If function types from algos shall be preserved, check resolveExpr function
-- | This function basically inlines function calls from inside the compile scope. It goes over all
--   algorithms loaded from the original file, extracts function calls from their code, retrieves the
--   code of the called functions from the registry and replaces a
--   @let x = fun(arg)@
--   by
--   @let fun = 'inlined function' in@
--   @     let x = fun arg@

resolveNS :: forall embExpr annot ty m anno.(MonadError Error m)
          => (Namespace (UnresolvedExpr embExpr annot ty) anno (OhuaType ty 'Resolved), NamespaceRegistry embExpr annot ty)
          -> m (Namespace (UnresolvedExpr embExpr annot ty) anno (OhuaType ty 'Resolved))
resolveNS (ns', registry) =
    return $ over algos (map (\algo -> over algoCode (work (view algoName algo) ) algo)) ns'
    where
        work :: Binding -> UnresolvedExpr embExpr annot ty -> UnresolvedExpr embExpr annot ty
        work algoNm algoExpr =
            let
                calledFunctions = collectCalledAlgos algoNm registry algoExpr
                expr = foldl (flip addExpr) algoExpr calledFunctions
                algoExpr' = resolveExpr expr
            in algoExpr'

        collectCalledAlgos :: Binding -> NamespaceRegistry embExpr annot ty -> UnresolvedExpr embExpr annot ty  -> HS.HashSet QualifiedBinding
        collectCalledAlgos algoNm availableAlgos expr  =
            let funsExceptAlgo = HS.filter (\funName -> funName /= QualifiedBinding (makeThrow []) algoNm) .  collectFunctionRefs $ expr
                
            in
                HS.unions .
                HS.toList .
                HS.map (\funName ->
                    maybe
                        HS.empty
                        (HS.insert funName . collectCalledAlgos algoNm (HM.delete funName availableAlgos))
                        $ HM.lookup funName availableAlgos <&> fst ) 
                $ funsExceptAlgo
            

        collectFunctionRefs :: UnresolvedExpr embExpr annot ty  -> HS.HashSet QualifiedBinding
        collectFunctionRefs e =
            -- FIXME: we need to resolve this reference here against the namespace and the registry (for Globs).
            -- We can/should not rely on algorithms being function literals or having function types here
            -- So we extract references and vars that are `applied` and match them against the algorithm register later
            HS.fromList $ [ fname |  AppE (LitE (FunRefLit (FunRef fname _fTy))) _annots _args <- flattenU e] 
                ++ [(QualifiedBinding (NSRef []) bnd) |  AppE (VarE bnd _ty) _annots _args <- flattenU e]
        
                        

        pathToVar :: QualifiedBinding -> Binding
        pathToVar (QualifiedBinding ns' bnd) =
            (makeThrow . T.intercalate ".") $ map unwrap $ unwrap ns' ++ [bnd]

        addExpr :: QualifiedBinding -> UnresolvedExpr embExpr annot ty  -> UnresolvedExpr embExpr annot ty 
        addExpr otherAlgo (LamE vars body) = LamE vars $ addExpr otherAlgo body
        addExpr otherAlgo e =
            -- (trace $ "Inlining Algo: "<> quickRender otherAlgo <> "\nwith type "<> show ty)
            let (algoExpr, algoTy) = fromMaybe (error "impossible") (HM.lookup otherAlgo registry)
                -- FIXME: Replace after function type unresolving and unit function work properly
                unresAlgoTy = fromMaybe TStar (resToUnres algoTy)

            in LetE
                    (VarP (pathToVar otherAlgo) unresAlgoTy)
                    algoExpr
                    e

        -- turns the function literal into a simple (var) binding
        replaceFunLit :: UnresolvedExpr embExpr annot ty -> UnresolvedExpr embExpr annot ty
        replaceFunLit = \case
            -- If the algorithm was a function literal before we need to replace it with a variable since it is bound in a local 
            -- let expression now, if it was a variable anyway, we have to do nothing
            LitE (FunRefLit (FunRef fName funTy)) | HM.member fName registry -> VarE (pathToVar fName) (FType funTy)
            e -> e

        resolveExpr :: UnresolvedExpr embExpr annot ty -> UnresolvedExpr embExpr annot ty
        resolveExpr = preWalkE replaceFunLit
