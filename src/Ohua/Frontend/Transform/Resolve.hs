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

import Ohua.Prelude

import Ohua.Frontend.Lang as FrLang (Expr(..), Pat(VarP), preWalkE, universeReplace, flatten)
import Ohua.Frontend.Types
import Ohua.Frontend.PPrint ()

import Control.Lens (over)
import qualified Data.HashMap.Strict as HM
import Data.Functor.Foldable (cata, embed)
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

-- QUESTION :  Am I getting this right?
resolveNS :: forall ty m anno.(MonadError Error m)
          => (Namespace (Expr ty Unresolved)  anno, NamespaceRegistry ty)
          -> m (Namespace (Expr ty Unresolved)  anno)
resolveNS (ns, registry) =
    return $ over algos (map (\algo -> over algoCode (work $ view algoName algo) algo)) ns
    where
        work :: Binding -> Expr ty Unresolved -> Expr ty Unresolved
        work algoNm algoExpr =
            let
                calledFunctions = collectAllFunctionRefs algoNm registry algoExpr
                expr = foldl (flip addExpr) algoExpr calledFunctions
                algoExpr' = resolveExpr expr
            in algoExpr'

        collectAllFunctionRefs :: Binding -> NamespaceRegistry ty -> Expr ty Unresolved -> HS.HashSet (QualifiedBinding, OhuaType ty Unresolved)
        collectAllFunctionRefs name available =
            HS.unions .
            HS.toList .
            HS.map (\(qb, ty) ->
                maybe
                    HS.empty
                    (HS.insert (qb, ty) . collectAllFunctionRefs name (HM.delete qb available))
                    $ HM.lookup qb available) .
            HS.filter (\(qb, ty )-> qb /= QualifiedBinding (makeThrow []) name) .  collectFunctionRefs

        collectFunctionRefs :: Expr ty Unresolved -> HS.HashSet (QualifiedBinding, OhuaType ty Unresolved)
        collectFunctionRefs e =
            -- FIXME we need to resolve this reference here against the namespace and the registry (for Globs).
            -- We can/should not rely on algorithms being function literals or having function types here
            -- So we extract references and vars that are `applied` and match them against the algorithm register later
            HS.fromList $ [ (fname, FType fTy) |  AppEU (LitE (FunRefLit (FunRef fname _ fTy))) _args <- flatten e] 
                ++ [(QualifiedBinding (NSRef []) bnd, ty) |  AppEU (VarE bnd ty) _args <- flatten e]
        
                        

        pathToVar :: QualifiedBinding -> Binding
        pathToVar (QualifiedBinding ns bnd) =
            (makeThrow . T.intercalate ".") $ map unwrap $ unwrap ns ++ [bnd]

        addExpr :: (QualifiedBinding, OhuaType ty Unresolved) -> Expr ty Unresolved -> Expr ty Unresolved
        -- TODO This is an assumption that fails in Ohua.Compile.Compiler.prepareRootAlgoVars
        --      We should enforce this via the type system rather than a runtime error!
        addExpr (otherAlgo, ty) (LamEU vars body) = LamEU vars $ addExpr  (otherAlgo, ty) body
        addExpr (otherAlgo, ty) e =
            -- (trace $"Adding Expression. Assign bind : "<> show bnd <> "\n to expression: "<> quickRender e)
            LetE
                (VarP (pathToVar otherAlgo) ty)
                (fromMaybe
                    (error "impossible") -- the path was originally retrieved from this list
                    $ HM.lookup otherAlgo registry)
                e

        -- turns the function literal into a simple (var) binding
        replaceFunLit :: Expr ty Unresolved -> Expr ty Unresolved
        replaceFunLit = \case
            -- If the algorithm was a function literal before we need to replace it with a variable since it is bound in a local 
            -- let expression now, if it was a variable anyway, we have to do nothing
            LitE (FunRefLit (FunRef fName _id funTy)) | HM.member fName registry -> VarE (pathToVar fName) (FType funTy)
            e -> e

        resolveExpr :: Expr ty Unresolved -> Expr ty Unresolved
        resolveExpr = preWalkE replaceFunLit
