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

import Ohua.Frontend.Lang as FrLang (Expr(LitE, LetE, VarE, LamE), Pat(VarP), ExprF(LitEF))
import Ohua.Frontend.Types
import Ohua.Frontend.PPrint ()

import Control.Lens (over)
import qualified Data.HashMap.Strict as HM
import Data.Functor.Foldable (cata, embed)
import qualified Data.HashSet as HS
import qualified Data.Text as T


-- | This function basically inlines function calls from inside the compile scope. It goes over all
--   algorithms loaded from the original file, extracts function calls from their code, retrievs the
--   code of the called functions from the registry and replaces a
--   @let x = fun(arg)@ 
--   by 
--   @let fun = 'inlined function' in@
--   @     let x = fun arg@

-- QUESTION :  Am I getting this right? 
resolveNS :: forall ty m anno.(MonadError Error m)
          => (Namespace (Expr ty) anno, NamespaceRegistry ty)
          -> m (Namespace (Expr ty) anno)
resolveNS (ns, registry) =
    return $ over algos (map (\algo -> over algoCode (work $ view algoName algo) algo)) ns
    where
        work :: Binding -> Expr ty -> Expr ty
        work name e =
            let
                calledFunctions = collectAllFunctionRefs name registry e
                expr = foldl (flip addExpr) e calledFunctions
                e' = resolveExpr expr
            in e'

        collectAllFunctionRefs :: Binding -> NamespaceRegistry ty -> Expr ty -> HS.HashSet QualifiedBinding
        collectAllFunctionRefs name available =
            HS.unions .
            HS.toList . 
            HS.map (\qb ->
                maybe 
                    HS.empty
                    (HS.insert qb . collectAllFunctionRefs name (HM.delete qb available))
                    $ HM.lookup qb available) . 
            HS.filter (/= QualifiedBinding (makeThrow []) name) .
            collectFunctionRefs

        collectFunctionRefs :: Expr ty -> HS.HashSet QualifiedBinding
        collectFunctionRefs e =
            -- FIXME we need to resolve this reference here against the namespace and the registry (for Globs).
            HS.fromList [r | LitE (FunRefLit (FunRef r _ _)) <- universe e]
        
        pathToVar :: QualifiedBinding -> Binding
        pathToVar (QualifiedBinding ns bnd) = 
            (makeThrow . T.intercalate ".") $ map unwrap $ unwrap ns ++ [bnd]
        
        addExpr :: QualifiedBinding -> Expr ty -> Expr ty
        -- TODO This is an assumption that fails in Ohua.Compile.Compiler.prepareRootAlgoVars
        --      We should enforce this via the type system rather than a runtime error!
        addExpr bnd (LamE vars body) = LamE vars $ addExpr bnd body
        addExpr bnd e = 
            LetE
                (VarP $ pathToVar bnd)
                (fromMaybe 
                    (error "impossible") -- the path was originally retrieved from this list
                    $ HM.lookup bnd registry)
                e

        -- turns the function literal into a simple (var) binding
        resolveExpr :: Expr ty -> Expr ty
        resolveExpr = cata $ \case
            LitEF (FunRefLit (FunRef ref _id _)) | HM.member ref registry -> VarE $ pathToVar ref
            e -> embed e
